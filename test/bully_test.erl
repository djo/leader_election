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

ping_leader_test_() ->
    [{"The node pings a leader",
      ?setup(fun ping_leader/1, #config{node = '2@127.0.0.1'})},
     {"The node starts announcing election on timeout",
      ?setup(fun start_announcing_on_timeout/1, #config{node = '2@127.0.0.1'})},
     {"The node starts leading as the highest identity on timeout",
      ?setup(fun start_leading_on_timeout/1, #config{node = '3@127.0.0.1'})}].

ping_test_() ->
    [{"The leader processes pings",
      ?setup(fun leader_processes_ping/1, #config{node = '1@127.0.0.1'})}].

pong_test_() ->
    [{"The node receives pong from the leader",
      ?setup(fun receive_pong/1, #config{node = '2@127.0.0.1'})}].

announce_election_test_() ->
    [{"The node starts announcing on an election announce",
      ?setup(fun start_announcing_on_announce/1, #config{node = '2@127.0.0.1'})},
     {"The node starts leading as the highest identity on an election announce",
      ?setup(fun start_leading_on_announce/1, #config{node = '3@127.0.0.1'})},
     {"The node keeps announcing on a new election announce",
      ?setup(fun keep_announcing_on_new_announce/1, #config{node = '2@127.0.0.1'})}].

check_announce_test_() ->
    [{"The node starts leading on timeout after an election announce",
      ?setup(fun start_leading_on_announce_timeout/1, #config{node = '2@127.0.0.1'})}].

ok_test_() ->
    [{"The node starts electing on announce approvement",
      ?setup(fun start_electing_on_announce_approvement/1, #config{node = '2@127.0.0.1'})},
     {"The node skips OK on pinging leader",
      ?setup(fun skip_ok_in_pinging_leader/1, #config{node = '2@127.0.0.1'})}].

check_electing_test_() ->
    [{"The node starts election announce again on electing timeout",
      ?setup(fun start_election_announce_again_on_timeout/1, #config{node = '2@127.0.0.1'})}].

new_leader_test_() ->
    [{"The node sets a new leader",
      ?setup(fun set_new_leader/1, #config{node = '2@127.0.0.1'})}].

%% ===================================================================
%% Actual tests
%% ===================================================================

ping_leader(#config{leader = Leader, timer_pid = TimerPid}) ->
    State0 = bully:get_state(),
    bully:cluster_event({pong, Leader}),
    State1 = bully:get_state(),
    {ok, Time, _Module, {timer, Event}} = timer_stub:trigger_timer(TimerPid),
    State2 = bully:get_state(),
    [?_assertEqual(?PINGING, State0#state.name),
     ?_assertEqual(?PINGED, State1#state.name),
     ?_assertEqual({4 * ?TIMEOUT, ping_leader}, {Time, Event}),
     ?_assertEqual(?PINGING, State2#state.name)].

start_announcing_on_timeout(#config{timer_pid = TimerPid}) ->
    State0 = bully:get_state(),
    {ok, Time, _Module, {timer, Event}} = timer_stub:trigger_timer(TimerPid),
    State1 = bully:get_state(),
    [?_assertEqual(?PINGING, State0#state.name),
     ?_assertEqual({4 * ?TIMEOUT, ping_leader}, {Time, Event}),
     ?_assertEqual(?ANNOUNCING, State1#state.name)].

start_leading_on_timeout(#config{timer_pid = TimerPid}) ->
    State0 = bully:get_state(),
    {ok, Time, _Module, {timer, Event}} = timer_stub:trigger_timer(TimerPid),
    State1 = bully:get_state(),
    [?_assertEqual(?PINGING, State0#state.name),
     ?_assertEqual('1@127.0.0.1', State0#state.leader),
     ?_assertEqual({4 * ?TIMEOUT, ping_leader}, {Time, Event}),
     ?_assertEqual(?LEADING, State1#state.name),
     ?_assertEqual('3@127.0.0.1', State1#state.leader)].

leader_processes_ping(_Config) ->
    State0 = bully:get_state(),
    bully:cluster_event({ping, '2@127.0.0.1'}),
    State1 = bully:get_state(),
    [?_assertEqual(?LEADING, State0#state.name),
     ?_assertEqual('1@127.0.0.1', State0#state.leader),
     ?_assertEqual(?LEADING, State1#state.name),
     ?_assertEqual('1@127.0.0.1', State1#state.leader)].

receive_pong(#config{leader = Leader}) ->
    State0 = bully:get_state(),
    bully:cluster_event({pong, Leader}),
    State1 = bully:get_state(),
    [?_assertEqual(?PINGING, State0#state.name),
     ?_assertEqual(?PINGED, State1#state.name)].

start_announcing_on_announce(_Config) ->
    State0 = bully:get_state(),
    bully:cluster_event({announce_election, '3@127.0.0.1'}),
    State1 = bully:get_state(),
    [?_assertEqual(?PINGING, State0#state.name),
     ?_assertEqual(?ANNOUNCING, State1#state.name)].

start_leading_on_announce(_Config) ->
    State0 = bully:get_state(),
    bully:cluster_event({announce_election, '2@127.0.0.1'}),
    State1 = bully:get_state(),
    [?_assertEqual(?PINGING, State0#state.name),
     ?_assertEqual('1@127.0.0.1', State0#state.leader),
     ?_assertEqual(?LEADING, State1#state.name),
     ?_assertEqual('3@127.0.0.1', State1#state.leader)].

keep_announcing_on_new_announce(_Config) ->
    State0 = bully:get_state(),
    bully:cluster_event({announce_election, '1@127.0.0.1'}),
    State1 = bully:get_state(),
    bully:cluster_event({announce_election, '3@127.0.0.1'}),
    State2 = bully:get_state(),
    [?_assertEqual(?PINGING, State0#state.name),
     ?_assertEqual(?ANNOUNCING, State1#state.name),
     ?_assertEqual(?ANNOUNCING, State2#state.name)].

start_leading_on_announce_timeout(#config{timer_pid = TimerPid}) ->
    State0 = bully:get_state(),
    bully:cluster_event({announce_election, '1@127.0.0.1'}),
    State1 = bully:get_state(),
    {ok, Time, _Module, {timer, Event}} = timer_stub:trigger_timer(TimerPid),
    State2 = bully:get_state(),
    [?_assertEqual(?PINGING, State0#state.name),
     ?_assertEqual('1@127.0.0.1', State0#state.leader),
     ?_assertEqual(?ANNOUNCING, State1#state.name),
     ?_assertEqual({?TIMEOUT, check_announce}, {Time, Event}),
     ?_assertEqual(?LEADING, State2#state.name),
     ?_assertEqual('2@127.0.0.1', State2#state.leader)].

start_electing_on_announce_approvement(_Config) ->
    State0 = bully:get_state(),
    bully:cluster_event({announce_election, '1@127.0.0.1'}),
    State1 = bully:get_state(),
    bully:cluster_event({ok, '3@127.0.0.1'}),
    State2 = bully:get_state(),
    [?_assertEqual(?PINGING, State0#state.name),
     ?_assertEqual(?ANNOUNCING, State1#state.name),
     ?_assertEqual(?ELECTING, State2#state.name)].

skip_ok_in_pinging_leader(_Config) ->
    State0 = bully:get_state(),
    bully:cluster_event({ok, '3@127.0.0.1'}),
    State1 = bully:get_state(),
    [?_assertEqual(?PINGING, State0#state.name),
     ?_assertEqual(?PINGING, State1#state.name)].

start_election_announce_again_on_timeout(#config{timer_pid = TimerPid}) ->
    State0 = bully:get_state(),
    bully:cluster_event({announce_election, '1@127.0.0.1'}),
    State1 = bully:get_state(),
    bully:cluster_event({ok, '3@127.0.0.1'}),
    State2 = bully:get_state(),
    {ok, Time, _Module, {timer, Event}} = timer_stub:trigger_timer(TimerPid),
    State3 = bully:get_state(),
    [?_assertEqual(?PINGING, State0#state.name),
     ?_assertEqual(?ANNOUNCING, State1#state.name),
     ?_assertEqual(?ELECTING, State2#state.name),
     ?_assertEqual({?TIMEOUT, check_electing}, {Time, Event}),
     ?_assertEqual(?ANNOUNCING, State3#state.name)].

set_new_leader(_Config) ->
    State0 = bully:get_state(),
    bully:cluster_event({new_leader, '3@127.0.0.1'}),
    State1 = bully:get_state(),
    [?_assertEqual(?PINGING, State0#state.name),
     ?_assertEqual('1@127.0.0.1', State0#state.leader),
     ?_assertEqual(?PINGING, State1#state.name),
     ?_assertEqual('3@127.0.0.1', State1#state.leader)].

%% ===================================================================
%% Setup functions
%% ===================================================================

start(#config{node = Node, leader = Leader} = Config) ->
    {ok, TimerPid} = timer_stub:start_link(),
    meck:new(timer, [unstick, passthrough]),
    meck:expect(timer, send_after, fun(Time, Pid, Msg) -> timer_stub:set_timer(TimerPid, {Time, Pid, Msg}) end),
    Nodes = lists:delete(Node, Config#config.nodes),
    bully:start_link(#state{leader = Leader, node = Node, nodes = Nodes, timeout = ?TIMEOUT}),
    Config#config{timer_pid = TimerPid, nodes = Nodes}.

stop(#config{timer_pid = TimerPid}) ->
    bully:stop(),
    meck:unload(timer),
    timer_stub:stop(TimerPid).
