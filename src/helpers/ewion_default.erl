-module(ewion_default).

-behaviour(ewion_behaviour_handle_request).

-export([handle_request/2]).

handle_request(Pid, Env) ->
    Headers = ewion_h:gv(headers, Env),
    Pid ! ewion:ok("Config not found for host: " ++ binary_to_list(ewion_h:gv('Host', Headers))).