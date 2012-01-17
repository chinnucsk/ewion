-module(ewion_adapter_misultin).

-behaviour(ewion_behaviour_adapter).

-export([handle_http/2, handle_chunked_response/1, get_env/1]).

%% ===================================================================
%% HTTP Handler
%% ===================================================================
handle_http(Req, ConfigModule) ->
    %% Get Host for routing
    Host = list_to_binary(ewion_h:gv('Host', Req:get(headers))),

    %% Get module to call for routing
    Module = ewion_h:gv(module, erlang:apply(ConfigModule, get_config, [Host])),

    %% Get Environment
    Env = get_env(Req),

    case erlang:apply(Module, handle_request, [self(), Env]) of
        chunked ->
            handle_chunked_response(Req);

        {Status, Headers, Data} ->
            Req:respond(Status, Headers, Data)
    end.

get_env(Req) ->
    Method = Req:get(method),
    [
        {method, Method},
        {path, element(2, Req:get(uri))},
        {args, ewion_h:proplists_adapter(get_req_args(Method, Req))},
        {cookies, ewion_h:proplists_adapter(Req:get_cookies())},
        {headers, ewion_h:headers_adapter(Req:get(headers))}
    ].

get_req_args('GET', Req) ->
    Req:parse_qs();

get_req_args('POST', Req) ->
    Req:parse_post().

%% ===================================================================
%% Handle Response
%% ===================================================================
handle_chunked_response(Req) ->
    receive
        {chunk, {_, Headers, Data}} ->
            Req:chunk(head, Headers),
            Req:chunk(Data),
            handle_chunked_response(Req);

        {chunk, Data} ->
            Req:chunk(Data),
            handle_chunked_response(Req);

        {end_chunk, Data} ->
            Req:chunk(Data),
            Req:chunk(done)
    end.