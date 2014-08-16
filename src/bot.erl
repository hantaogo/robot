-module(bot).

-export ([
	in/0,
	in/1, 
	in/3,
	out/0
	]).

-export([
	go/0,
	start/0,
	stop/0
	]).
in() ->
	{ok, {Count, Times, Delay}} = application:get_env(bot, join),
	in(Count, Times, Delay).

out() ->
	robot_master:clear().

in(Count, 1, _) ->
	in(Count);
in(Count, Times, Delay) ->
	in(Count),
	timer:apply_after(Delay, ?MODULE, in, [Count, Times-1, Delay]).

in(Count) ->
	% io:format("~n------------add ~p robot------------~n", [Count]),
	{ok, NamePre} = application:get_env(bot, name_pre),
	F = fun(N) ->
		[S] = io_lib:format("~p", [N]),
		string:concat(NamePre, S)
	end,
	Last = robot_master:count(),
	{ok, {Success, Fail}} = robot_master:add_many(lists:map(F, lists:seq(Last+1, Last+Count))),
	% {ok, {Success, Fail}}. 
	{ok, {length(Success), length(Fail)}}. 
	
go() ->
	Result = application:start(bot),
	in(),
	Result.

start() ->
	application:start(bot).
	
stop() ->
	out(),
	application:stop(bot).

