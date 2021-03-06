-module(robot_master).
-behaviour(gen_server).

-export([
	start_link/0,
	count/0,
	success_count/0,
	add/1,
	add_many/1,
	clear/0,
	random_name/1
	]).

-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
	]).

-record(state, {users :: dict(), success_count :: integer(), surnames, boynames, grilnames}).

% ----------------------------------------------------------------------------
%                                  API
% ----------------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

clear() ->
	gen_server:call(?MODULE, clear).

add(User) ->
	gen_server:call(?MODULE, {add, User}).

add_many(UserList) ->
	gen_server:call(?MODULE, {add_many, UserList}).

count() ->
	gen_server:call(?MODULE, count).

success_count() ->
	gen_server:call(?MODULE, success_count).

random_name(Sex) ->
	gen_server:call(?MODULE, {random_name, Sex}).

% ----------------------------------------------------------------------------
%                             gen_server callbacks
% ----------------------------------------------------------------------------
init([]) ->
	{ok, BinSurname} = file:read_file("name/surnames.txt"),
	{ok, BinBoyname} = file:read_file("name/boynames.txt"),
	{ok, BinGrilname} = file:read_file("name/grilnames.txt"),
	Surnames = string:tokens(unicode:characters_to_list(BinSurname, utf8), [13,10]),
	Boynames = string:tokens(unicode:characters_to_list(BinBoyname, utf8), [13,10]),
	Grilnames = string:tokens(unicode:characters_to_list(BinGrilname, utf8), [13,10]),
	io:format("Surname count: ~p~n", [length(Surnames)]),
	io:format("Boynames count: ~p~n", [length(Boynames)]),
	io:format("Grilnames count: ~p~n", [length(Grilnames)]),
	{ok, #state{users=dict:new(), success_count=0, surnames=Surnames, boynames=Boynames, grilnames=Grilnames}}.

handle_call(count, _From, #state{users=Users}=State) ->
	{reply, dict:size(Users), State};

handle_call(success_count, _From, #state{success_count=C}=State) ->
	{reply, C, State};

handle_call({add, User}, _From, #state{users=Users}=State) ->
	case dict:find(User, Users) of
		{ok, _} ->
			{reply, {error, already_exists}, State};
		error ->
			{ok, Pid} = robot_sup:start_robot(User),
			{reply, {ok, Pid}, State#state{users=dict:store(User, Pid, Users)}}
	end;

handle_call({add_many, UserList}, _From, #state{users=Users}=State) ->
	F = fun(User, {Success, Fail, Dict}) ->
		case dict:find(User, Dict) of
			{ok, _} ->
				{Success, Fail ++ [User], Dict};
			error ->
				case robot_sup:start_robot(User) of
					{ok, Pid} ->
						{Success ++ [User], Fail, dict:store(User, Pid, Dict)};
					_Other ->
						{Success, Fail ++ [User], Dict}
				end
		end
	end,
	{Success, Fail, Dict} = lists:foldl(F, {[],[],Users}, UserList),
	{reply, {ok, {Success, Fail}}, State#state{users=Dict}};

handle_call(clear, _From, #state{users=Users}=State) ->
	F = fun({_User, Pid}) ->
			robot_sup:stop_robot(Pid)
		end,
	lists:foreach(F, dict:to_list(Users)),
	{reply, ok, State#state{users=dict:new()}};

handle_call({random_name, boy}, _From, #state{surnames=Surnames, boynames=Boynames}=State) ->
	{reply, {ok, utils:utf(utils:random_from_list(Surnames) ++ utils:random_from_list(Boynames))}, State};

handle_call({random_name, gril}, _From, #state{surnames=Surnames, grilnames=Grilnames}=State) ->
	{reply, {ok, utils:utf(utils:random_from_list(Surnames) ++ utils:random_from_list(Grilnames))}, State};

handle_call({random_name, _}, _From, #state{}=State) ->
	{reply, {error, invalid_sex}, State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({success, _User}, #state{success_count=Count}=State) ->
	{noreply, State#state{success_count=Count+1}};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(Reason, _State) ->
	error_logger:format("~p terminate for reason: ~p~n", [?MODULE, Reason]).
	
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
