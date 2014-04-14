%%% @doc Module stubbing library for erlang:send_after.

-module(timer_stub).

-behaviour(gen_server).

-record(state, {
        time :: integer(),
        timer_pid :: pid(),
        message :: term()
}).

%% API
-export([start_link/0,
         stop/1,
         set_timer/2,
         trigger_timer/1]).

%% gen_server callbacks
-export([init/1,
         handle_cast/2,
         handle_info/2,
         handle_call/3,
         terminate/2,
         code_change/3]).

%% ===================================================================
%% Module API
%% ===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

-spec(set_timer(pid(), {integer(), pid(), term()}) -> ok).
set_timer(Pid, {Time, TimerPid, Message}) ->
    gen_server:cast(Pid, {set_timer, Time, TimerPid, Message}),
    ok.

-spec(trigger_timer(pid()) -> {ok, integer(), pid(), term()}).
trigger_timer(Pid) ->
    gen_server:call(Pid, trigger_timer).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    {ok, #state{message = {}}}.

handle_call(trigger_timer, _From, #state{message = Message} = State) when Message =/= {} ->
    TimerPid = State#state.timer_pid,
    TimerPid ! Message,
    {reply, {ok, State#state.time, TimerPid, Message}, State#state{message = {}}};

handle_call(Request, From, State) ->
    erlang:error(unexpected_call, [Request, From, State]).

handle_cast({set_timer, Time, TimerPid, NewMessage}, #state{message = Message} = State) when Message =:= {} ->
    {noreply, State#state{time = Time, timer_pid = TimerPid, message = NewMessage}};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Event, State) ->
    erlang:error(unexpected_cast, [Event, State]).

handle_info(Request, State) ->
    erlang:error(unexpected_info, [Request, State]).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
