-module (connecter).
-behaviour (gen_server).

-export ([
	start_link/3,
	start/0,
	cast/3,
	call/4
	]).

% implement gen_server
-export ([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
	]).

% internal
-export ([
	m_send/2,
	m_csend/3,
	m_cast/3,
	m_call/4,
	m_ccast/4,
	m_ccall/5
	]).

-record (state, {pid, socket, key}).

-define (SERVICE_PING, 1).
-define (SERVICE_SENDKEY, 5).

-define (DEFAULT_KEY, [99,104,114,100,119]).

% ----------------------------------------------------------------------------
%                                  API
% ----------------------------------------------------------------------------
start_link(Pid, Host, Port) ->
	gen_server:start_link(?MODULE, [Pid, Host, Port], []).

start() ->
	gen_server:start(?MODULE, [self(), "127.0.0.1", 7766], []).

cast(Ref, ServiceId, Msg) ->
	gen_server:cast(Ref, {ccast, ServiceId, Msg}).

call(Ref, ServiceId, MsgId, Msg) ->
	gen_server:cast(Ref, {ccall, ServiceId, MsgId, Msg}).

% ----------------------------------------------------------------------------
%                            implement gen_server
% ----------------------------------------------------------------------------
init([Pid, Host, Port]) ->
	% io:format("new connecter: ~p,~p,~p,~p~n", [self(), Pid, Host, Port]),
	{ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, true}]),
	% send tell me codekey
	m_cast(Socket, ?SERVICE_SENDKEY, <<>>),
	{ok, #state{pid=Pid, socket=Socket, key=?DEFAULT_KEY}}.
	
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast({ccast, ServiceId, Msg}, #state{socket=Socket, key=Key} = State) ->
	m_ccast(Socket, Key, ServiceId, Msg),
	{noreply, State};

handle_cast({ccall, ServiceId, MsgId, Msg}, #state{socket=Socket, key=Key} = State) ->
	m_ccall(Socket, Key, ServiceId, MsgId, Msg),
	{noreply, State};

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info({tcp, _Socket, <<_Len:32/integer, 3, _StrLen:16/integer, CodeKey/binary>>}, #state{pid=Pid, key=Key}=State) ->
	% io:format("key: ~p~n", [Key]),
	NewKey = codekit:coding(binary_to_list(CodeKey), Key),
	Pid ! {ready, self()},
	% io:format("update key: ~p~n", [NewKey]),
	erlang:start_timer(0, self(), ping),
	{noreply, State#state{key=NewKey}};

handle_info({tcp, _Socket, <<7:32/integer,0,_:32/integer,200:16/integer>>}, State) ->
	% io:format("receive ping~n", []),
	{noreply, State};

handle_info({tcp, _Socket, <<Data/binary>>}, #state{pid=Pid}=State) ->
	% io:format("receive: ~p~n", [Data]),
	Pid ! {recv, self(), Data},
	{noreply, State};

handle_info({timeout, _TimerRef, ping}, #state{socket=Socket, key=Key} = State) ->
	% repeat send ping message
	% io:format("ping...~n"),
	m_ccall(Socket, Key, ?SERVICE_PING, 0, <<>>),
	erlang:start_timer(120000, self(), ping),
	{noreply, State};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(Reason, _State) ->
	io:format("client terminate: ~p~n", [Reason]).

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% ----------------------------------------------------------------------------
%                                 internal
% ----------------------------------------------------------------------------
m_send(Socket, Bin) ->
	% io:format("send : ~p~n", [Bin]),
	gen_tcp:send(Socket, Bin).

m_csend(Socket, Key, <<Len:32/integer, ServiceId, Bin/binary>>) ->
	CodeData = list_to_binary(codekit:coding(binary_to_list(Bin), Key)),
	FinalData = <<Len:32/integer, ServiceId, CodeData/binary>>,
	% io:format("csend old: ~p~n", [Bin]), 
	% io:format("csend: ~p~n", [FinalData]),
	gen_tcp:send(Socket, FinalData).

m_cast(Socket, ServiceId, Msg) ->
	Data = <<ServiceId, Msg/binary>>,
	Len = size(Data),
	m_send(Socket, <<Len:32/integer, Data/binary>>).

m_call(Socket, ServiceId, MsgId, Msg) ->
	Data = <<ServiceId, MsgId:32/integer, Msg/binary>>,
	Len = size(Data),
	m_send(Socket, <<Len:32/integer, Data/binary>>).

m_ccast(Socket, Key, ServiceId, Msg) ->
	Data = <<ServiceId, Msg/binary>>,
	Len = size(Data),
	m_csend(Socket, Key, <<Len:32/integer, Data/binary>>).

m_ccall(Socket, Key, ServiceId, MsgId, Msg) ->
	Data = <<ServiceId, MsgId:32/integer, Msg/binary>>,
	Len = size(Data),
	m_csend(Socket, Key, <<Len:32/integer, Data/binary>>).