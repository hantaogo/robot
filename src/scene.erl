-module(scene).
-include("grid.hrl").
-include("scene.hrl").
-behaviour(gen_server).

-export([
	start/0,
	stop/0,
	start_link/1,
	filename/1,
	string_id/1,
	scenes/0,
	get_scene/1,
	path/3,
	random_walkable/1
	]).

-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
	]).

-record(state, {path, scenes}).

% ----------------------------------------------------------------------------
%                                  API
% ----------------------------------------------------------------------------
start() ->
	start_link("scene").

stop() ->
	gen_server:cast(?MODULE, stop).

start_link(Path) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Path], []).
 
-spec filename(integer()) -> {ok, string()} | error.
filename(Tid) ->
	gen_server:call(?MODULE, {filename, Tid}).

-spec string_id(integer()) -> {ok, string()} | error.
string_id(Tid) ->
	gen_server:call(?MODULE, {string_id, Tid}).

-spec scenes() -> list().
scenes() ->
	gen_server:call(?MODULE, scenes).

-spec get_scene(integer()) -> term().
get_scene(Tid) ->
	gen_server:call(?MODULE, {get_scene, Tid}).

-spec path(integer(), point(), point()) -> points().
path(Tid, A, B) ->
	gen_server:call(?MODULE, {path, Tid, A, B}).

-spec random_walkable(integer()) -> point().
random_walkable(Tid) ->
	gen_server:call(?MODULE, {random_walkable, Tid}).

% ----------------------------------------------------------------------------
%                             gen_server callbacks
% ----------------------------------------------------------------------------
init([Path]) ->
	case filelib:is_dir(Path) of
		false ->
			{stop, normal};
		true ->
			F = fun(Filename, {Count, Scenes}) ->
				try
					Scene = load_map(Filename),
					{Count+1, dict:append(Scene#scene.tid, Scene, Scenes)}
				catch
					_:Error ->
						io:format("load scene ~p error: ~p~n", [Filename, Error]),
						{Count+1, Scenes}
				end
			end,
			{_Count, Scenes} = filelib:fold_files(Path, "^\\w+\\.txt$", false, F, {0, dict:new()}),
			% io:format("load file count: ~p    scene count: ~p~n", [Count, length(dict:fetch_keys(Scenes))]),
			% 设置随机种子
			{A1, A2, A3} = now(),
			random:seed(A1, A2, A3),
			{ok, #state{path=Path, scenes=Scenes}}
	end.

handle_call({filename, Tid}, _From, #state{path=Path, scenes=Scenes}=State) ->
	case dict:find(Tid, Scenes) of
		{ok, Scene} ->
			{reply, {ok, filename:join(Path, Scene#scene.filename)}, State};
		error ->
			{reply, error, State}
	end;

handle_call({string_id, Tid}, _From, #state{scenes=Scenes}=State) ->
	case dict:find(Tid, Scenes) of
		{ok, Scene} ->
			{reply, {ok, Scene#scene.string_id}, State};
		error ->
			{reply, error, State}
	end;

handle_call({get_scene, Tid}, _From, #state{scenes=Scenes}=State) ->
	case dict:find(Tid, Scenes) of
		{ok, Scene} ->
			{reply, {ok, Scene}, State};
		error ->
			{reply, error, State}
	end;

handle_call({path, Tid, A, B}, _From, #state{scenes=Scenes}=State) ->
	case dict:find(Tid, Scenes) of
		{ok, [#scene{grid=G}]} ->
			{reply, {ok, grid:find_path(A,B,G)}, State};
		error ->
			{reply, error, State}
	end;

handle_call({random_walkable, Tid}, _From, #state{scenes=Scenes}=State) ->
	case dict:find(Tid, Scenes) of
		{ok, [#scene{grid=G}]} ->
			P = grid:random_walkable(G),
			{reply, {ok, P}, State};
		error ->
			{reply, error, State}
	end;

handle_call(scenes, _From, #state{scenes=Scenes}=State) ->
	{reply, dict:to_list(Scenes), State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(Reason, _State) ->
	error_logger:format("~p terminate for reason: ~p~n", [?MODULE, Reason]).

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

% ----------------------------------------------------------------------------
%                                  internal
% ----------------------------------------------------------------------------

-spec load_map(string()) -> term().
load_map(Filename) ->
	Basename = filename:basename(Filename),
	StringId = filename:rootname(Basename),
	{ok, Data} = file:read_file(Filename),
	Tid = utils:hashcode(StringId),
	Grid = grid:make(Data),
	% {W,H} = grid:size(Grid),
	% io:format("~p ~p (~px~p)~n", [Tid, StringId, W, H]),
	#scene{tid=Tid, string_id=StringId, filename=Basename, grid=Grid}.
