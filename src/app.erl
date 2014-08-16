-module(app).

-behaviour(application).
-export([start/2,stop/1]).


start(_Type, _StartArgs) ->
	sup:start_link().

stop(_State) ->
    sup:stop().