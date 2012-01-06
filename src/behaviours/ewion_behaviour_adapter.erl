-module(ewion_behaviour_adapter).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{handle_http,2}, {get_env,1}, {handle_response,1}, {handle_chunked_response,1}];

behaviour_info(_Other) ->
    undefined.