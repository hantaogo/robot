-module(sup).
-behaviour(supervisor).

%% External exports
-export([
    start_link/0
    ]).

%% supervisor callbacks
-export([
    init/1
    ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	
    RestartStrategy = one_for_all,
    MaxRestarts = 1,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    
    SceneSup = {scene_sup, {scene_sup, start_link, []}, permanent, infinity, supervisor, [scene_sup]},
    ChaterSup = {chater_sup, {chater_sup, start_link, []}, permanent, infinity, supervisor, [chater_sup]},
    RobotSup = {robot_sup, {robot_sup, start_link, []}, permanent, infinity, supervisor, [robot_sup]},
    RobotMaster = {robot_master, {robot_master, start_link, []}, permanent, 500, worker, [robot_master]},

    Children = [SceneSup, ChaterSup, RobotMaster, RobotSup],
    {ok, {SupFlags, Children}}.
