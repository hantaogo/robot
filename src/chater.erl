-module(chater).
-behaviour(gen_server).

-export([
	start_link/2,
	stop/0,
	response/2,
	register/2,
	unregister/1
	]).

-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
	]).

-record(state, {socket, chars}).

% ----------------------------------------------------------------------------
%                                  API
% ----------------------------------------------------------------------------
start_link(Host, Port) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Host, Port], []).

stop() ->
	gen_server:cast(?MODULE, stop).

response(Name, Content) ->
	gen_server:cast(?MODULE, {response, Name, Content}).

register(Name, Pid) ->
	gen_server:call(?MODULE, {register, Name, Pid}).

unregister(Name) ->
	gen_server:call(?MODULE, {unregister, Name}).

% ----------------------------------------------------------------------------
%                             gen_server callbacks
% ----------------------------------------------------------------------------
init([Host, Port]) ->
	case gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, true}]) of
		{ok, Socket} ->
			{ok, #state{socket=Socket, chars=dict:new()}};
		{error, _Reason} ->
			{ok, #state{chars=dict:new()}}
	end.

handle_call({register, Name, Pid}, _From, #state{chars=Chars}=State) ->
	{reply, ok, State#state{chars=dict:store(Name, Pid, Chars)}};

handle_call({unregister, Name}, _From, #state{chars=Chars}=State) ->
	{reply, ok, State#state{chars=dict:erase(Name, Chars)}};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast({response, _Name, _Content}, #state{socket=undefined}=State) ->
	{noreply, State};

handle_cast({response, Name, Content}, #state{socket=Socket}=State) ->
	Data = list_to_binary(Name++":"++Content),
	% io:format("Name: ~p Content: ~p Data: ~p~n", [Name, Content, Data]),
	gen_tcp:send(Socket, <<Data/binary>>),
	{noreply, State};

handle_cast(stop, #state{socket=undefined}=State) ->
	{stop, normal, State};

handle_cast(stop, #state{socket=Socket}=State) ->
	gen_tcp:send(Socket, <<"88">>),
	{stop, normal, State};

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({tcp, _Socket, <<Data/binary>>}, #state{chars=Chars}=State) ->
	% io:format("tcp: ~p~n", [Data]),
	S = binary_to_list(Data),
	case catch string:tokens(S, ":") of
		[Name, Content] ->
			case dict:find(Name, Chars) of
				{ok, Pid} ->
					Pid ! {chat, list_to_binary(Content)};
				error ->
					error
			end;
		_Error ->
			ok
	end,
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(Reason, _State) ->
	io:format("~p terminate, reason: ~p~n", [self(), Reason]).

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
