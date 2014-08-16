-module(scene_sup).
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
    
    {ok, ScenePath} = application:get_env(bot, scenepath),
    ChildSpec = {scene, {scene, start_link, [ScenePath]}, permanent, 500, worker, [scene]},
    
    Children = [ChildSpec],
    {ok, {SupFlags, Children}}.

