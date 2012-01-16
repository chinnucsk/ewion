Erlang webserver interface

#Overview
Ewion have one main function:

__Server__ = atom()  
__ConfigModule__ = atom()

    handle_http(Server, ConfigModule)

It returns what servers expected. For example, misultin expects fun() and cowboy expects dispatch rules.

####Misultin example

    % start misultin http server
    start(Port) ->
    	misultin:start_link([{port, Port}, {loop, ewion:handle_http(misultin, test_config_module}]).

####Cowboy

    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    cowboy:start_listener(http, 100,
        cowboy_tcp_transport, [{port, 8080}],
        cowboy_http_protocol, [{dispatch, ewion:handle_http(cowboy, test_config_module)}]
    ).

**ConfigModule** is a module with one exported function:

__Host__ = binary()

    get_config(Host)

For example:

    get_config(<<"127.0.0.1">>) ->
        [{module, test_handler}];

    %%Optional
    get_config(_Host) ->
        [{module, ewion_default}].

It takes **Module** and call handle_request function:

####handle_request

__Pid__ = pid()  
__Env__ = proplist()

    handle_request(Pid, Env)

Examples Env:

__method__ = atom()  
__path__ = list()  
__args__ = proplist({binary(), binary()})  
__cookies__ = proplist({binary(), binary()})  
__headers__ = proplists({atom(), binary()})

    {method, 'GET'},
    {path, "/test/path"},
    {args, []},
    {cookies, [{<<"test_cookie">>, <<"1">>}]},
    {headers, [{'Host', <<"127.0.0.1">>}]}

You must send to Pid response in format:

    {Status, Headers, Data}

or

    {chunk, {Status, Headers, Data}} for chunked response. Look at source.


You also can use __ok__ function from ewion module:

__Data__ = list()

    ok(Data)

Return {200, [{'Content-Type', <<"text/html">>}], Data}.

with headers:

__Headers__ = proplist({atom(), binary()})
__Data__ = list()

    ok(Headers, Data)

Return {200, [{'Content-Type', <<"text/html">>}|Headers], Data}.

#Behaviours

ewion_behaviour_adapter - for adapter

ewion_behaviour_handle_request - for module which exports handle_request

#Helpers

**ewion_h** module contains some helpers function:

#### Resource

Return a list of resources from path.

__Url__ = list()

    resource(Url)

Return for example: "/test/path" = ["test", "path"]

#TESTS

Ewion Test servers: <https://github.com/8Protons/ewion_test>

# Example

    %% ===================================================================
    %% Ewion callbacks
    %% ===================================================================
    handle_request(Pid, Env) ->
        Method = proplists:get_value(method, Env),
        Path = proplists:get_value(path, Env),
        Cookies = proplists:get_value(cookies, Env),

        %% Middlewares
        %% Get Session
        ID = get_session_id(proplists:get_value(<<"site_session">>, Cookies)),
        Session = get_session(ID),

        handle(Pid, Method, ewion_h:resource(Path), Env, Session).

Handle function:

    handle(Pid, 'GET', ["test", "path"], _, _) ->
        Pid ! {200, [{'Content-Type', <<"text/html">>}], <<"Ok! Super-Duper">>};

or

    handle(Pid, 'GET', ["test", "path"], _, _) ->
        Pid ! ewion:ok(<<"Ok! Super-Duper">>);
