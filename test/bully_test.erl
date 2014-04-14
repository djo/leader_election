-module(bully_test).
-include_lib("eunit/include/eunit.hrl").

-include("state.hrl").

-record(config, {
        node :: atom(),
        leader :: atom(),
        timer_pid :: pid()
}).

-define(TIMEOUT, 1).
-define(setup(F, Config), {setup, fun() -> start(Config) end, fun stop/1, F}).

%% ===================================================================
%% Tests descriptions
%% ===================================================================

ping_pong_test_() ->
    [{"The server pings a leader",
      ?setup(fun ping_pong_with_leader/1, #config{node = '1@127.0.0.1', leader = '2@127.0.0.1'})},
     {"The leader processes pings",
      ?setup(fun ping_pong_as_leader/1, #config{node = '1@127.0.0.1', leader = '1@127.0.0.1'})}].

announce_election_test_() ->
    [{"The server starts leading on timout",
      ?setup(fun start_leading_on_timeout/1, #config{node = '2@127.0.0.1', leader = '1@127.0.0.1'})}].

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

start_leading_on_timeout(#config{timer_pid = TimerPid}) ->
    State0 = bully:get_state(),
    {ok, Time, _Module, {timer, Event}} = timer_stub:trigger_timer(TimerPid),
    State1 = bully:get_state(),
    [?_assertEqual(?PINGING, State0#state.name),
     ?_assertEqual({4 * ?TIMEOUT, ping_leader}, {Time, Event}),
     ?_assertEqual(?LEADING,  State1#state.name)].

%% ===================================================================
%% Setup functions
%% ===================================================================

start(#config{node = Node, leader = Leader} = Config) ->
    {ok, TimerPid} = timer_stub:start_link(),
    meck:new(timer, [unstick, passthrough]),
    meck:expect(timer, send_after, fun(Time, Pid, Msg) -> timer_stub:set_timer(TimerPid, {Time, Pid, Msg}) end),
    net_kernel:start([Node, longnames]),
    bully:start_link(#state{leader = Leader, timeout = ?TIMEOUT}),
    Config#config{timer_pid = TimerPid}.

stop(#config{timer_pid = TimerPid}) ->
    bully:stop(),
    net_kernel:stop(),
    meck:unload(timer),
    timer_stub:stop(TimerPid).
