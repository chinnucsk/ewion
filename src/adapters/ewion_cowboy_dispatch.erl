-module(ewion_cowboy_dispatch).

-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, Opts) ->
    [ConfigModule|_] = Opts,
	{ok, Req, ConfigModule}.

handle(Req, State) ->
    ewion_adapter_cowboy:handle_http(Req, State).    

terminate(_Req, _State) ->
	ok.