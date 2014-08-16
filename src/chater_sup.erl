-module(chater_sup).
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
    
    {ok, {ChatIp, ChatPort}} = application:get_env(bot, chat),
    ChildSpec = {chater, {chater, start_link, [ChatIp, ChatPort]}, permanent, 500, worker, [chater]},
    
    Children = [ChildSpec],
    {ok, {SupFlags, Children}}.

