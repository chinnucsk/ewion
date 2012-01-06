-module(ewion_h).

%% Path helper
-export([resource/1]).

%% Data Helpers
-export([gv/2, gv/3, string_kv_to_binary_kv/1, string_kv_to_binary_kv/2]).

%% ===================================================================
%% Rest
%% ===================================================================
resource(Url) ->
	string:tokens(string:to_lower(Url), "/").

%% ===================================================================
%% Get Value (GV)
%% ===================================================================
gv(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        {K,V} when K =:= Key ->
            V;
        _ ->
            undefined
    end.

gv(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        {K,V} when K =:= Key ->
            V;
        _ ->
            Default
    end.

%% ===================================================================
%% Proplist to binary
%% ===================================================================
string_kv_to_binary_kv([]) ->
    [];
string_kv_to_binary_kv(List) ->
    string_kv_to_binary_kv(List, []).

string_kv_to_binary_kv([{K, V}|T], Acc) ->
    string_kv_to_binary_kv(T, [{cast_value(K), cast_value(V)} | Acc]);

string_kv_to_binary_kv([], Acc) ->
    Acc.

cast_value(Value) when is_atom(Value) ->
    Value;
cast_value(Value) when is_list(Value) ->
    list_to_binary(Value).