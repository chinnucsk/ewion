-module(ewion_h).

%% Path helper
-export([resource/1]).

%% Data Helpers
-export([gv/2, gv/3]).

%% Atom Helpers
-export([lowercase/1]).

%% Data Adapters
-export([headers_adapter/1, proplists_adapter/1]).

%% ===================================================================
%% Rest
%% ===================================================================
resource(Url) when is_list(Url) ->
  Tokens = string:tokens(string:to_lower(Url), "/"),
  [list_to_binary(Elem) || Elem <- Tokens];

resource(Url) when is_binary(Url) ->
  resource(binary_to_list(Url));

resource(_) ->
  [].

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
%% Atom to lowercase
%% ===================================================================
lowercase(Value) when is_atom(Value) ->
  list_to_atom(string:to_lower(atom_to_list(Value))).

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