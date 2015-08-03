-module(erlZenDeskStats_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("records.hrl").
%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
   ?Log("erlZenDeskStats_sup:init",[]),
    RestartStrategy = one_for_one,

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    Worker = {erlZenDeskStats_worker, {erlZenDeskStats_worker, start_link, []},
              Restart, Shutdown, Type, [erlZenDeskStats_worker]},
    {ok, { {RestartStrategy, 5, 10}, [Worker]} }.

