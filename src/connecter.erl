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
	case gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, true}]) of
		{ok, Socket} ->
			% send tell me codekey
			m_ccast(Socket, ?DEFAULT_KEY, ?SERVICE_SENDKEY, <<>>),
			{ok, #state{pid=Pid, socket=Socket, key=?DEFAULT_KEY}};
		{error, Reason} ->
			{stop, Reason}
	end.
	
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

handle_info({tcp, _Socket, Data}, State) ->
	% io:format("recv ~p~n", [Data]),
	% case codekit:decode(Data) of
	% 	error ->
	% 		io:format("decode fail: ~p~n", [Data]),
	% 		{noreply, State};
	% 	{ok, Bin} ->
	% 		% io:format("receive data: ~p~n", [Bin]),
	% 		handle_info_tcp(Bin, State)
	% end;
	<<_Len:32/integer, Bin/binary>> = Data,
	handle_info_tcp(Bin, State);

handle_info({tcp_closed, _Socket}, #state{pid=Pid}=State) ->
	Pid ! {tcp_closed, self()},
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
	Len = size(Bin),
	gen_tcp:send(Socket, <<Len:32/integer, Bin/binary>>).

m_csend(Socket, Key, <<ServiceId, Bin/binary>>) ->
	EncryptData = list_to_binary(codekit:coding(binary_to_list(Bin), Key)),
	EncodeData = <<ServiceId, EncryptData/binary>>,
	FinalData = codekit:encode(EncodeData),
	% io:format("csend to sercive: ~p ~p~n", [ServiceId, Bin]), 
	% io:format("csend: ~p~n", [FinalData]),
	gen_tcp:send(Socket, FinalData).

m_cast(Socket, ServiceId, Msg) ->
	Data = <<ServiceId, Msg/binary>>,
	m_send(Socket, <<Data/binary>>).

m_call(Socket, ServiceId, MsgId, Msg) ->
	Data = <<ServiceId, MsgId:32/integer, Msg/binary>>,
	m_send(Socket, <<Data/binary>>).

m_ccast(Socket, Key, ServiceId, Msg) ->
	Data = <<ServiceId, Msg/binary>>,
	m_csend(Socket, Key, <<Data/binary>>).

m_ccall(Socket, Key, ServiceId, MsgId, Msg) ->
	Data = <<ServiceId, MsgId:32/integer, Msg/binary>>,
	m_csend(Socket, Key, <<Data/binary>>).


handle_info_tcp(<<3, _StrLen:16/integer, CodeKey/binary>>, #state{pid=Pid, key=Key}=State) ->
	% io:format("key: ~p~n", [Key]),
	NewKey = codekit:coding(binary_to_list(CodeKey), Key),
	Pid ! {ready, self()},
	% io:format("update key: ~p~n", [NewKey]),
	erlang:start_timer(0, self(), ping),
	{noreply, State#state{key=NewKey}};

% 屏蔽Ping消息
handle_info_tcp(<<0, _:32/integer,200:16/integer>>, State) ->
	{noreply, State};

handle_info_tcp(<<Data/binary>>, #state{pid=Pid}=State) ->
	Pid ! {recv, self(), Data},
	{noreply, State}.