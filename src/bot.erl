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

-export([
	g/0,
	s/2,
	c/2,
	ds/2,
	dc/2,
	p/0,
	ps/2,
	pc/2
	]).

in() ->
	{ok, {Count, Times, Delay}} = application:get_env(bot, join),
	in(Count, Times, Delay).

out() ->
	chater:stop(),
	robot_master:clear().

in(Count, 1, _) ->
	in(Count);
in(Count, Times, Delay) ->
	% io:format("~p~n", [Times]),
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
	Result = start(),
	in(),
	Result.

start() ->
	application:start(bot).
	
stop() ->
	out(),
	application:stop(bot).

g() ->
	ok = bot:start(),
	{ok, P} = robot_master:add("gm1"),
	register(g, P).

-spec s(integer(), binary()) -> term().
s(ServiceId, Msg) ->
	robot:cast(g, ServiceId, Msg).

-spec c(integer(), binary()) -> term().
c(ServiceId, Msg) ->
	robot:call(g, ServiceId, 99, Msg).

-spec ds(integer(), binary()) -> term().
ds(ServiceId, Msg) ->
	robot:castdc(g, ServiceId, Msg).

-spec dc(integer(), binary()) -> term().
dc(ServiceId, Msg) ->
	robot:calldc(g, ServiceId, 99, Msg).

p() ->
	{ok, P} = connecter:start("127.0.0.1", 17766),
	register(p, P).

ps(ServiceId, Msg) ->
	connecter:ncast(p, ServiceId, Msg).

pc(ServiceId, Msg) ->
	connecter:ncast(p, ServiceId, 99, Msg).