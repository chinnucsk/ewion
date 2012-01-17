-module(ewion_h).

%% Path helper
-export([resource/1]).

%% Data Helpers
-export([gv/2, gv/3]).

%% Data Adapters
-export([headers_adapter/1, proplists_adapter/1]).

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
%% Headers Adapter
%% ===================================================================
headers_adapter([]) ->
    [];
headers_adapter(List) ->
    [{to_atom(Key), to_binary(Value)} || {Key, Value} <- List].

%% ===================================================================
%% Headers Adapter
%% ===================================================================
proplists_adapter([]) ->
    [];
proplists_adapter(List) ->
    [{to_binary(Key), to_binary(Value)} || {Key, Value} <- List].

%% ===================================================================
%% to_ helpers
%% ===================================================================
to_atom(Value) when is_atom(Value) ->
    Value;
to_atom(Value) when is_list(Value) ->
    list_to_atom(Value);
to_atom(Value) when is_binary(Value) ->
    binary_to_atom(Value, latin1);
to_atom(_Value) ->
    undefined.

to_binary(Value) when is_binary(Value) ->
    Value;
to_binary(Value) when is_atom(Value) ->
    atom_to_binary(Value, latin1);
to_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
to_binary(_Value) ->
    <<"">>.