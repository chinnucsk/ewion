-module(ewion_adapter_cowboy).

-behaviour(ewion_behaviour_adapter).

-export([handle_http/2, handle_chunked_response/1, get_env/1]).

%% ===================================================================
%% HTTP Handler
%% ===================================================================
handle_http(Req, ConfigModule) ->
    %% Get Host for routing
    case element(1, cowboy_http_req:port(Req)) of
        80 ->
            Host = element(1, cowboy_http_req:raw_host(Req));

        Port ->
            RawHost = element(1, cowboy_http_req:raw_host(Req)),
            BPort = list_to_binary(integer_to_list(Port)),
            Host = <<RawHost/binary, ":", BPort/binary>>
    end,

    %% Get module to call for routing
    {Module, Node} = erlang:apply(ConfigModule, get_request_handler, [Host]),

    %% Get Environment
    Env = get_env(Req),

    case rpc:call(Node, Module, handle_request, [{self(), node()}, Env]) of
        {chunked} ->
            handle_chunked_response(Req);

        {response, Res} ->
            handle_response(Req, ConfigModule, Res);
    
        {badrpc, Stack} ->
            case rpc:call(Node, Module, handle_error, [{self(), node()}, Env, Stack]) of
                {response, Res} ->
                    handle_response(Req, ConfigModule, Res)
            end            
    end.

handle_response(Req, State, {Status, Headers, Data}) ->
    {ok, Req2} = cowboy_http_req:reply(Status, Headers, Data, Req),
    {ok, Req2, State}.

get_env(Req) ->
    Method = element(1, cowboy_http_req:method(Req)),    
    [
        {method, element(1, cowboy_http_req:method(Req))},
        {path, element(1, cowboy_http_req:raw_path(Req))},
        {args, get_req_args(Method, Req)},
        {cookies, element(1, cowboy_http_req:cookies(Req))},
        {headers, element(1, cowboy_http_req:headers(Req))}
    ].

get_req_args('GET', Req) ->
    element(1, cowboy_http_req:qs_vals(Req));

get_req_args('POST', Req) ->
    element(1, cowboy_http_req:body_qs(Req)).

%% ===================================================================
%% Handle Chunked Response
%% ===================================================================
handle_chunked_response(Req) ->
    receive
        {chunk, {Status, Headers, Data}} ->
            cowboy_http_req:chunked_reply(Status, Headers, Req),
            cowboy_http_req:chunk(Data),
            handle_chunked_response(Req);

        {chunk, Data} ->
            cowboy_http_req:chunk(Data),
            handle_chunked_response(Req);

        {end_chunk, Data} ->
            Req:chunk(Data),
            Req:chunk(done)
    end.