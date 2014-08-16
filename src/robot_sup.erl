-module(robot_sup).
-behaviour(supervisor).

%% External exports
-export([
    start_link/0,
    start_robot/1,
    stop_robot/1
    ]).

%% supervisor callbacks
-export([
    init/1
    ]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    
    ChildSpec = {ignore, {robot, start_link, []}, temporary, 100, worker, [robot]},
    
    Children = [ChildSpec],
    {ok, {SupFlags, Children}}.


start_robot(Username) ->
    supervisor:start_child(?MODULE, [Username]).

stop_robot(Pid) ->
    Pid ! stop,
    supervisor:terminate_child(?MODULE, Pid),
    supervisor:delete_child(?MODULE, Pid),
    ok.