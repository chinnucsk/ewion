-module(ewion).

-export([handle_http/2]).

%% Response Helpers
-export([ok/1, ok/2]).

%% ===================================================================
%% HTTP Handler
%% ===================================================================
handle_http(cowboy, ConfigModule) ->
    [{'_', [{'_', ewion_cowboy_dispatch, [ConfigModule]}]}];

handle_http(misultin, ConfigModule) ->
    fun(Req) -> ewion_adapter_misultin:handle_http(Req, ConfigModule) end.

%% ===================================================================
%% Response helpers
%% ===================================================================
ok(Data) ->
    {200, [{'Content-Type', <<"text/html">>}], Data}.

ok(Headers, Data) ->
    {200, [{'Content-Type', <<"text/html">>}|Headers], Data}.