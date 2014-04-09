-module(bully_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Module API
%% ===================================================================

start() ->
    lager:start(),
    ok = application:start(bully),
    connect_nodes(),
    ok.

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    bully_sup:start_link().

stop(_State) ->
    ok = application:stop(lager),
    ok = application:stop(leader).

%% ===================================================================
%% Internal functions
%% ===================================================================

connect_nodes() ->
    {ok, Nodes0} = application:get_env(bully, nodes),
    Nodes = lists:delete(node(), Nodes0),
    lager:info("Try to connect nodes ~p", [Nodes]),
    Results = lists:map(fun net_kernel:connect_node/1, Nodes),
    case lists:member(true, Results) of
        true -> lager:info("Connected to nodes ~p", [nodes()]);
        false -> lager:info("No connected nodes")
    end,
    ok.
