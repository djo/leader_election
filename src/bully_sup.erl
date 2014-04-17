-module(bully_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% State record
-include("state.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, Timeout} = application:get_env(bully, timeout),
    {ok, Leader} = application:get_env(bully, leader),
    {ok, Nodes0} = application:get_env(bully, nodes),
    Nodes = lists:delete(node(), Nodes0),
    State = #state{leader = Leader, node = node(), nodes = Nodes, timeout = Timeout},
    {ok, { {one_for_one, 5, 10}, [?CHILD(bully, worker, [State])]} }.
