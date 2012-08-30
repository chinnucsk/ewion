-module(ewion_default).

-behaviour(ewion_behaviour_handle_request).

-export([handle_request/1, handle_error/2]).

handle_request(Env) ->
  Headers = ewion_h:gv(headers, Env),
  ewion:html("Config not found for host: " ++ binary_to_list(ewion_h:gv('Host', Headers))).

handle_error(_Env, _Stack) ->
  ewion:html("Something goes wrong").
