-module(ewion).

-export([handle_http/2]).

%% Response Helpers
-export([html/1, html/2, redirect/1]).

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
html(Data) ->
    {response, {200, [{'Content-Type', <<"text/html">>}], Data}}.

html(Headers, Data) ->
    {response, {200, [{'Content-Type', <<"text/html">>}|Headers], Data}}.

redirect(Url) ->
    {response, {302, [{'Location', Url}], <<"">>}}.