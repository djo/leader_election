-module(bully_test).
-include_lib("eunit/include/eunit.hrl").

-include("state.hrl").

-record(config, {
        node :: atom(),
        leader = '1@127.0.0.1' :: atom(),
        nodes = ['1@127.0.0.1', '2@127.0.0.1', '3@127.0.0.1'] :: [atom()],
        timer_pid :: pid()
}).

-define(TIMEOUT, 1).
-define(setup(F, Config), {setup, fun() -> start(Config) end, fun stop/1, F}).

%% ===================================================================
%% Tests descriptions
%% ===================================================================

ping_pong_test_() ->
    [{"The server pings a leader",
      ?setup(fun ping_pong_with_leader/1, #config{node = '2@127.0.0.1'})},
     {"The leader processes pings",
      ?setup(fun ping_pong_as_leader/1, #config{node = '1@127.0.0.1'})}].

ping_leader_timeout_test_() ->
    [{"The server starts electing on timeout",
      ?setup(fun start_electing_on_timeout/1, #config{node = '2@127.0.0.1'})},
     {"The server starts leading as the highest identity on timeout",
      ?setup(fun start_leading_on_timeout/1, #config{node = '3@127.0.0.1'})},
     {"The server starts leading without any available node on timeout",
      ?setup(fun start_leading_alone_on_timeout/1, #config{node = '2@127.0.0.1', nodes = []})}].

election_announce_test_() ->
    [{"The server starts electing on announce",
      ?setup(fun start_electing_on_announce/1, #config{node = '2@127.0.0.1'})},
     {"The server starts leading as the highest identity on announce",
      ?setup(fun start_leading_on_announce/1, #config{node = '3@127.0.0.1'})}].

set_new_leader_test_() ->
    [{"The server sets a new leader",
      ?setup(fun set_new_leader/1, #config{node = '2@127.0.0.1'})}].

%% ===================================================================
%% Actual tests
%% ===================================================================

ping_pong_with_leader(#config{leader = Leader, timer_pid = TimerPid}) ->
    State0 = bully:get_state(),
    bully:cluster_event({pong, Leader}),
    State1 = bully:get_state(),
    {ok, Time, _Module, {timer, Event}} = timer_stub:trigger_timer(TimerPid),
    State2 = bully:get_state(),
    [?_assertEqual(?PINGING, State0#state.name),
     ?_assertEqual(?PINGED,  State1#state.name),
     ?_assertEqual({4 * ?TIMEOUT, ping_leader}, {Time, Event}),
     ?_assertEqual(?PINGING, State2#state.name)].

ping_pong_as_leader(_Config) ->
    State0 = bully:get_state(),
    bully:cluster_event({ping, '2@127.0.0.1'}),
    State1 = bully:get_state(),
    [?_assertEqual(?LEADING, State0#state.name),
     ?_assertEqual(?LEADING, State1#state.name)].

start_electing_on_timeout(#config{timer_pid = TimerPid}) ->
    State0 = bully:get_state(),
    {ok, Time, _Module, {timer, Event}} = timer_stub:trigger_timer(TimerPid),
    State1 = bully:get_state(),
    [?_assertEqual(?PINGING, State0#state.name),
     ?_assertEqual({4 * ?TIMEOUT, ping_leader}, {Time, Event}),
     ?_assertEqual(?ELECTING,  State1#state.name)].

start_leading_on_timeout(#config{timer_pid = TimerPid}) ->
    State0 = bully:get_state(),
    {ok, Time, _Module, {timer, Event}} = timer_stub:trigger_timer(TimerPid),
    State1 = bully:get_state(),
    [?_assertEqual(?PINGING, State0#state.name),
     ?_assertEqual({4 * ?TIMEOUT, ping_leader}, {Time, Event}),
     ?_assertEqual(?LEADING,  State1#state.name)].

start_leading_alone_on_timeout(#config{timer_pid = TimerPid}) ->
    State0 = bully:get_state(),
    {ok, Time, _Module, {timer, Event}} = timer_stub:trigger_timer(TimerPid),
    State1 = bully:get_state(),
    [?_assertEqual(?PINGING, State0#state.name),
     ?_assertEqual({4 * ?TIMEOUT, ping_leader}, {Time, Event}),
     ?_assertEqual(?LEADING,  State1#state.name)].

start_electing_on_announce(_Config) ->
    State0 = bully:get_state(),
    bully:cluster_event({announce_election, '3@127.0.0.1'}),
    State1 = bully:get_state(),
    [?_assertEqual(?PINGING, State0#state.name),
     ?_assertEqual(?ELECTING,  State1#state.name)].

start_leading_on_announce(_Config) ->
    State0 = bully:get_state(),
    bully:cluster_event({announce_election, '2@127.0.0.1'}),
    State1 = bully:get_state(),
    [?_assertEqual(?PINGING, State0#state.name),
     ?_assertEqual(?LEADING,  State1#state.name)].

set_new_leader(#config{timer_pid = TimerPid}) ->
    State0 = bully:get_state(),
    {ok, Time, _Module, {timer, Event}} = timer_stub:trigger_timer(TimerPid),
    bully:cluster_event({new_leader, '3@127.0.0.1'}),
    State1 = bully:get_state(),
    [?_assertEqual(?PINGING, State0#state.name),
     ?_assertEqual('1@127.0.0.1', State0#state.leader),
     ?_assertEqual({4 * ?TIMEOUT, ping_leader}, {Time, Event}),
     ?_assertEqual(?PINGING,  State1#state.name),
     ?_assertEqual('3@127.0.0.1', State1#state.leader)].

%% ===================================================================
%% Setup functions
%% ===================================================================

start(#config{node = Node, leader = Leader} = Config) ->
    {ok, TimerPid} = timer_stub:start_link(),
    meck:new(timer, [unstick, passthrough]),
    meck:expect(timer, send_after, fun(Time, Pid, Msg) -> timer_stub:set_timer(TimerPid, {Time, Pid, Msg}) end),
    net_kernel:start([Node, longnames]),
    Nodes = lists:delete(Node, Config#config.nodes),
    bully:start_link(#state{leader = Leader, nodes = Nodes, timeout = ?TIMEOUT}),
    Config#config{timer_pid = TimerPid, nodes = Nodes}.

stop(#config{timer_pid = TimerPid}) ->
    bully:stop(),
    net_kernel:stop(),
    meck:unload(timer),
    timer_stub:stop(TimerPid).
