%% -*- coding: utf-8 -*-

-module (robot).
-behaviour (gen_fsm).

%% 外部接口
-export ([
	start_link/1,
	start/1,
	stop/1,
	move/3
	]).

% gen_fsm 回调函数
-export ([
	init/1,
	handle_event/3,
	handle_sync_event/4,
	handle_info/3,
	terminate/3,
	code_change/4
	]).

% -define (DC_HOST, "127.0.0.1").
% -define (DC_PORT, 7766).
% -define (GAME_HOST, "127.0.0.1").
% -define (GAME_PORT, 6766).
% -define (KEY, "XCWXCWXCWXXX").

-define (DC_SERVICE_LOGIN, 2).
-define (DC_SERVICE_HERO_UPDATE, 10).
-define (GAME_SERVICE_LOGIN, 2).
-define (GAME_SERVICE_JOIN, 11).
-define (GAME_SERVICE_SCENE, 12).
-define (GAME_SERVICE_CHAT, 105).

-define (DC_MSG_ID_LOGIN, 0).
-define (DC_MSG_ID_CREATE_ROLE, 1).
-define (GAME_MSG_ID_LOGIN, 0).
-define (GAME_MSG_ID_QUIT, 1).

-define (TYPE_SCENE_ALL_INFO, 6).
-define (TYPE_SCENE_OBJECT_INFO, 2).
-define (TYPE_SCENE_HERO_SELF, 15).

-define (TIME_MOVE, 200).
% -define (TIME_THINK, 10000).

-define (CHANNEL_SCENE, 8).

-record (data, {user, dc, game, is_dc_ready, is_game_ready, hero, scene, timer, name, role_id, session_id}).
-record (hero, {type, show_id, camp_id, head_id, confraternity, offical_id, is_god_army, level, heading, move_speed, x, y, char_name, parameter_id, attack_mode}).
-record (scene, {tid}).

% gen_fsm 所有状态
-export ([
	not_ready/2,
	not_entergame/2,
	just_enter/2,
	wait/2
	]).

% ----------------------------------------------------------------------------
%                                  API
% ----------------------------------------------------------------------------
start_link(User) ->
	gen_fsm:start_link(?MODULE, [User], []).

start(User) ->
	gen_fsm:start(?MODULE, [User], []).

stop(Pid) ->
	gen_fsm:send_all_state_event(Pid, stop).

move(Ref, X, Y)->
	gen_fsm:send_event(Ref, {move, X, Y}).

% ----------------------------------------------------------------------------
%                                  gen_fsm 回调函数
% ----------------------------------------------------------------------------
init([User]) ->
	% io:format("new robot ~p~n", [User]),
	{ok, {DcIp, DcPort}} = application:get_env(bot, dc),
	{ok, {GameIp, GamePort}} = application:get_env(bot, game),
	{ok, DcPid} = connecter:start_link(self(), DcIp, DcPort),
	{ok, GamePid} = connecter:start_link(self(), GameIp, GamePort),
	% 重新设置随机种子
	{A1, A2, A3} = now(),
	random:seed(A1, A2, A3),
	{ok, not_ready, #data{user=User, dc=DcPid, game=GamePid, is_dc_ready=false, is_game_ready=false}}.

handle_event(_Event, Name, Data) -> 
	{next_state, Name, Data}.

handle_sync_event(_Event, _From, Name, Data) ->
	{reply, ok, Name, Data}.

handle_info({ready, X}, Name, Data) ->
	gen_fsm:send_event(self(), {ready, X}),
	{next_state, Name, Data};

handle_info({recv, X, Bin}, Name, Data) ->
	gen_fsm:send_event(self(), {recv, X, Bin}),
	{next_state, Name, Data};

handle_info({timeout, TimerRef, What}, Name, Data) ->
	gen_fsm:send_event(self(), {timeout, TimerRef, What}),
	{next_state, Name, Data};

handle_info(think, Name, Data) ->
	gen_fsm:send_event(self(), think),
	{next_state, Name, Data};

handle_info({chat, Bin}, Name, Data) ->
	gen_fsm:send_event(self(), {chat, Bin}),
	{next_state, Name, Data};

handle_info(stop, _Name, Data) ->
	{stop, normal, Data};

handle_info(_Info, Name, #data{user=_User}=Data) ->
	% io:format("~p handle_info ~p~n", [User, Info]),
	{next_state, Name, Data}.

terminate(Reason, Name, #data{user=User, game=Game, timer=Timer, hero=Hero}) ->
	io:format("~p terminate in state ~p reason: ~p~n", [User, Name, Reason]),
	% 退出游戏
	CMD_QUIT = 2,
	Msg = <<CMD_QUIT>>,
	connecter:call(Game, ?GAME_SERVICE_LOGIN, ?GAME_MSG_ID_QUIT, Msg),
	% 取消计时器
	timer:cancel(Timer),
	chater:unregister(Hero#hero.char_name).

code_change(_OldVsn, Name, Data, _Extra) ->
	{ok, Name, Data}.

% ----------------------------------------------------------------------------
%                                  所有状态
% ----------------------------------------------------------------------------

not_ready({ready, Dc}, #data{dc=Dc}=Data) ->
	not_ready_check(Data#data{is_dc_ready=true});

not_ready({ready, Game}, #data{game=Game}=Data) ->
	not_ready_check(Data#data{is_game_ready=true});
	
not_ready(Event, Data) ->
	io:format("not_ready not process event: ~p~n", [Event]),
	{next_state, not_ready, Data}.

not_entergame({recv, Dc, <<_Len:32/integer, 0, ?DC_MSG_ID_LOGIN:32/integer, 200:16/integer, 0, SessionIdLen:16/integer, SessionId:SessionIdLen/binary, _/binary>>}, #data{user=User, dc=Dc}=Data) ->
	% io:format("~p not created role, session id: ~p~n", [User, SessionId]),
	% 创建人物
	CMD_CREATE_ROLE = 1,
	Username = utils:utf(User++"&"),
	Sid = utils:utf(SessionId),
	Name = make_name(User),
	Camp = 1,
	L = [{1,1,1},{1,0,2},{2,1,3},{2,0,4},{3,1,5},{3,0,6},{4,1,7},{4,0,8}],
	{Occupation, Sex, Head} = random_from_list(L),
	Msg = <<CMD_CREATE_ROLE, Username/binary, Sid/binary, Name/binary, Camp, Occupation, Sex, Head>>,
	
	% io:format("create role ~p~n", [User]),
	connecter:call(Dc, ?DC_SERVICE_HERO_UPDATE, ?DC_MSG_ID_CREATE_ROLE, Msg),
	{next_state, not_entergame, Data#data{session_id=SessionId}};

not_entergame({recv, Dc, <<_Len:32/integer, 0, ?DC_MSG_ID_CREATE_ROLE:32/integer, 200:16/integer, NameLen:16/integer, NameBin:NameLen/binary, RoleId:32/integer, _Sex, _Camp, _Grade, _Selected, _Race, _LiveState>>}, #data{user=User, dc=Dc, session_id=SessionId, game=Game}=Data) ->
	% 创建人物成功，进入游戏
	% io:format("role created: ~p ~p~n", [NameBin, RoleId]),
	% io:format("enter game~n"),
	CMD_LOGIN = 1,
	Username = utils:utf(User ++ "&"),
	Name = <<NameLen:16/integer, NameBin/binary>>,
	Sid = utils:utf(SessionId),
	Msg = <<CMD_LOGIN, Username/binary, Name/binary, RoleId:32/integer, Sid/binary>>,
	
	connecter:call(Game, ?GAME_SERVICE_LOGIN, ?GAME_MSG_ID_LOGIN, Msg),
	{next_state, not_entergame, Data#data{name=NameBin, role_id=RoleId}};

not_entergame({recv, Dc, <<_Len:32/integer, 0, ?DC_MSG_ID_LOGIN:32/integer, 200:16/integer, _RoleCount, CharsData/binary>>}, #data{user=User, dc=Dc, game=Game}=Data) ->
	% io:format("~p char count ~p data: ~p~n", [User, RoleCount, CharsData]),
	<<NameLen:16/integer, NameBin:NameLen/binary, RoleId:32/integer, _Sex, _Camp, _Grade, _Selected, _Race, _LiveState, SessionIdLen:16/integer, SessionId:SessionIdLen/binary, _/binary>> = CharsData,
	% io:format("dc login ok -> name: ~p, roleId: ~p, sessionId: ~p~n", [NameBin, RoleId, SessionId]),
	CMD_LOGIN = 1,
	Username = utils:utf(User ++ "&"),
	Name = <<NameLen:16/integer, NameBin/binary>>,
	Sid = <<SessionIdLen:16/integer, SessionId/binary>>,
	Msg = <<CMD_LOGIN, Username/binary, Name/binary, RoleId:32/integer, Sid/binary>>,
	connecter:call(Game, ?GAME_SERVICE_LOGIN, ?GAME_MSG_ID_LOGIN, Msg),
	{next_state, not_entergame, Data#data{name=NameBin, role_id=RoleId, session_id=SessionId}};

not_entergame({recv, Game, <<_Len:32/integer, 0, ?GAME_MSG_ID_LOGIN:32/integer, 200:16/integer, _Other/binary>>}, #data{game=Game, user=_User}=Data) ->
	% io:format("~p login game ok, start join in world!~n", [User]),
	CMD_LOGIN = 21,
	Msg = <<CMD_LOGIN>>,
	connecter:cast(Game, ?GAME_SERVICE_JOIN, Msg),
	{next_state, just_enter, Data};

% 场景消息
not_entergame({recv, Dc, _Bin}, #data{user=_User, dc=Dc}=Data) ->
	% io:format("~p not_entergame not process dc msg: ~p~n", [User, Bin]),
	{next_state, not_entergame, Data};

not_entergame({recv, Game, _Bin}, #data{user=_User, game=Game}=Data) ->
	% io:format("~p not_entergame not process game msg: ~p~n", [User, Bin]),
	{next_state, not_entergame, Data};

not_entergame(Event, #data{user=User}=Data) ->
	io:format("~p not_entergame not process event: ~p~n", [User, Event]),
	{next_state, not_entergame, Data}.

just_enter({recv, Game, <<_Len:32/integer, ?GAME_SERVICE_SCENE, ?TYPE_SCENE_ALL_INFO, Bin/binary>>}, #data{user=_User, game=Game}=Data) ->
	% io:format("recv scene info: ~p~n", [Bin]),
	% io:format("~p recv scene info 1~n", [User]),
	{ok, Scene} = bin_to_scene(Bin),
	just_enter_check(Data#data{scene=Scene});

just_enter({recv, Game, <<_Len:32/integer, ?GAME_SERVICE_SCENE, ?TYPE_SCENE_OBJECT_INFO, Tid:32/integer, _Bin/binary>>}, #data{user=_User, game=Game}=Data) ->
	% io:format("recv scene info: ~p~n", [Bin]),
	% io:format("~p recv scene info 2~n", [User]),
	just_enter_check(Data#data{scene=#scene{tid=Tid}});

just_enter({recv, Game, <<_Len:32/integer, ?GAME_SERVICE_SCENE, ?TYPE_SCENE_HERO_SELF, Bin/binary>>}, #data{user=_User, game=Game}=Data) ->
	% io:format("recv hero info: ~p~n", [Bin]),
	% io:format("~p recv hero info~n", [User]),
	{ok, Hero} = bin_to_hero(Bin),
	just_enter_check(Data#data{hero=Hero});

just_enter(_Event, #data{user=_User}=Data) ->
	% io:format("~p just_enter not process event: ~p~n", [User, Event]),
	{next_state, just_enter, Data}.

wait({move, X, Y}, #data{scene=Scene, hero=Hero}=Data) ->
	% io:format("~p ~p,~p --> ~p,~p~n", [Hero#hero.char_name, Hero#hero.x, Hero#hero.y, X, Y]),
	case scene:path(Scene#scene.tid, {Hero#hero.x, Hero#hero.y}, {X, Y}) of
		{ok, Path} ->
			% io:format("~p~n", [Path]),
			erlang:start_timer(?TIME_MOVE, self(), {move, Path});
		error ->
			io:format("fail to find path: ~p ~p,~p --> ~p~p~n", [Hero#hero.char_name, Hero#hero.x, Hero#hero.y, X, Y])
	end,
	{next_state, wait, Data};

wait(think, #data{scene=Scene, hero=Hero, game=_Game}=Data) ->
	% io:format("~p think~n", [Hero#hero.char_name]),
	% 随机移动
	case catch scene:random_walkable(Scene#scene.tid) of
		{ok, {X, Y}} ->
			X0 = Hero#hero.x,
			Y0 = Hero#hero.y,
			Dx = X0 - X,
			Dy = Y0 - Y,
			case Dx * Dx + Dy * Dy =< 400 of
				true ->
					% io:format("~p ~p ~p,~p --> ~p,~p~n", [Scene#scene.tid, Hero#hero.char_name, Hero#hero.x, Hero#hero.y, X, Y]),
					case scene:path(Scene#scene.tid, {Hero#hero.x, Hero#hero.y}, {X, Y}) of
						{ok, L} ->
							% io:format("found path: ~p~n", [L]),
							erlang:start_timer(?TIME_MOVE, self(), {move, L});
						error ->
							io:format("fail to find path: ~p ~p,~p --> ~p~p~n", [Hero#hero.char_name, Hero#hero.x, Hero#hero.y, X, Y])
					end;
				false ->
					ok
			end;
		_Error ->
			io:format("~p find path fail with random_walkable!~n", [Hero#hero.char_name])
	end,
	% say(Game, Hero#hero.char_name, make_word()),
	{next_state, wait, Data};

wait({recv, Game, <<_Len:32/integer, ?GAME_SERVICE_CHAT, ?CHANNEL_SCENE, _Vip, NameLen:16/integer, _Name:NameLen/binary, ContentLen:16/integer, Content:ContentLen/binary, _Other/binary>>}, #data{user=User, game=Game}=Data) ->
	chater:response(User, binary_to_list(Content)),
	{next_state, wait, Data};

wait({chat, Content}, #data{hero=Hero, game=Game}=Data) ->
	% io:format("chat: ~p~n", [Content]),
	say(Game, Hero#hero.char_name, Content),
	{next_state, wait, Data};

% 行走
wait({timeout, _TimerRef, {move, [{X,Y}|RestPath]}}, #data{game=Game, scene=Scene, hero=Hero}=Data) ->
	Tid = Scene#scene.tid,
	Msg = <<4, Tid:32/integer, X:16/integer, Y:16/integer>>,
	connecter:cast(Game, ?GAME_SERVICE_SCENE, Msg),
	erlang:start_timer(?TIME_MOVE, self(), {move, RestPath}),
	% io:format("move: ~p,~p~n", [X, Y]),
	{next_state, wait, Data#data{hero=Hero#hero{x=X, y=Y}}};

wait({timeout, _TimerRef, {move, []}}, #data{hero=_Hero}=Data) ->
	% io:format("[-> wait] ~p arrived ~p,~p~n", [Hero#hero.char_name, Hero#hero.x, Hero#hero.y]),
	{next_state, wait, Data};

wait(_Event, Data) ->
	% io:format("wait not process event: ~p~n", [Event]),
	{next_state, wait, Data}.

% ----------------------------------------------------------------------------
%                                  内部的
% ----------------------------------------------------------------------------

% 检测帐号服务器和游戏服务器是否都连接完毕，如果都完毕则开始登录
not_ready_check(#data{user=User, dc=Dc}=Data) ->
	case Data#data.is_dc_ready andalso Data#data.is_game_ready of
		true ->
			CMD_LOGIN = 1,
			Username = utils:utf(User++"&"),
			Time = utils:seconds1970(),
			Stime = utils:i2s(Time),
			{ok, Key} = application:get_env(bot, key),
			Sign = utils:md5(User++Stime++Key),
			Password = utils:utf("_1username=" ++ User ++ "&time=" ++ Stime ++ "&sign=" ++ Sign),
			Msg = <<CMD_LOGIN, Username/binary, Password/binary>>,
			connecter:call(Dc, ?DC_SERVICE_LOGIN, ?DC_MSG_ID_LOGIN, Msg),
			{next_state, not_entergame, Data};
		_ ->
			{next_state, not_ready, Data}
	end.

% 检测刚进入游戏时，英雄和场景信息是否都获取完毕，如果都完毕，则进入等待状态
just_enter_check(#data{user=User, hero=Hero}=Data) ->
	case Data#data.hero /= undefined andalso Data#data.scene /= undefined of
		true ->
			io:format("  ~p join~n", [binary_to_list(Hero#hero.char_name)]),
			{ok, TimeThink} = application:get_env(bot, time_think),
			{ok, Timer} = timer:send_interval(TimeThink, self(), think),
			% 立即触发一次思考
			% self() ! think,
			robot_master ! {success, User},
			% 在聊天服务器里注册
			chater:register(binary_to_list(Hero#hero.char_name), self()),
			{next_state, wait, Data#data{timer=Timer}};
		_ ->
			{next_state, just_enter, Data}
	end.

-spec bin_to_hero(binary()) -> {ok, term()} | {error, term()}.
bin_to_hero(<<Type, ShowId:32/integer, CampId, HeadId, ConfLen:16/integer, ConfName:ConfLen/binary, OfficalId:16/integer, IsGodArmy, Level:16/integer, Heading, MoveSpeed:16/integer, X:16/integer, Y:16/integer, CharNameLen:16/integer, CharName:CharNameLen/binary, ParameterId:32/integer, AttackMode, _/binary>>) ->
	{ok, #hero{type=Type, show_id=ShowId, camp_id=CampId, head_id=HeadId, confraternity=ConfName, offical_id=OfficalId, is_god_army=IsGodArmy, level=Level, heading=Heading, move_speed=MoveSpeed, x=X, y=Y, char_name=CharName, parameter_id=ParameterId, attack_mode=AttackMode}};
bin_to_hero(Bin) ->
	{error, {invalid_data, Bin}}.

-spec bin_to_scene(binary()) -> {ok, term()} | {error, term()}.
bin_to_scene(<<Tid:32/integer, _/binary>>) ->
	{ok, #scene{tid=Tid}};
bin_to_scene(Bin) ->
	{error, {invalid_data, Bin}}.

make_name(User) ->
	utils:utf(User).

% make_word() ->
% 	L = [
% 	"有这么一个规律：胸小的姑娘一般脾气都特大；胸大的姑娘一般脾气都特好。古语云：穷胸极恶，有容奶大。 ", 
% 	"刚看到邻居大嫂在打孩子，我问:怎么了，干嘛打他？邻居大嫂:这孩子太过分了！不打不行！我：怎么过分了？大嫂：这倒霉孩子，要吃月饼！我：至于吗？孩子想吃块月饼，你就打他？大嫂：他非要吃韭菜鸡蛋的！该……打死才好涅… ", 
% 	"看到一句话：在感情没到那份上的时候，你想爱人家好好爱就是了，爱就是嘘寒问暖，是犬马之诚，是投以木李，报之琼玖，匪报而永以为好也。什么欲擒故纵，什么声东击西，什么知我者谓我心忧，不知我者谓我何求。别学那些没用的，初级就只能做任务，被爱的人才有资格用技能。 ", 
% 	"男人永远不懂女人的经痛,女人也永远不会了解男人的蛋疼. ",
% 	"一个人吃饭的时候会感到孤单，但一个人吃零食的时候就不会。 "
% 	],
% 	random_from_list(L).

random_from_list(L) ->
	A = array:from_list(L),
	I = random:uniform(array:size(A))-1,
	array:get(I, A).
	
say(Game, CharName, Content) ->
	Cannel = 8,
	VipLevel = 0,
	CharNameWithLen = utils:utf(CharName),
	ContentWithLen = utils:utf(Content),
	Msg = <<Cannel, VipLevel, CharNameWithLen/binary, ContentWithLen/binary, 0>>,
	connecter:cast(Game, ?GAME_SERVICE_CHAT, Msg).