%%% @doc Module stubbing library for erlang:send_after.

-module(timer_stub).

-behaviour(gen_server).

-record(state, {
        time :: integer(),
        pid :: pid(),
        message :: term()
}).

%% API
-export([start_link/0,
         stop/0,
         set_timer/3,
         trigger_timer/0]).

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
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

-spec(set_timer(integer(), pid(), term()) -> ok).
set_timer(Time, Pid, Message) ->
    gen_server:cast(?MODULE, {set_timer, Time, Pid, Message}),
    ok.

-spec(trigger_timer() -> {ok, integer(), pid(), term()}).
trigger_timer() ->
    gen_server:call(?MODULE, trigger_timer).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    {ok, #state{message = {}}}.

handle_call(trigger_timer, _From, #state{message = Message} = State) when Message =/= {} ->
    Pid = State#state.pid,
    Pid ! Message,
    {reply, {ok, State#state.time, Pid, Message}, State#state{message = {}}};

handle_call(Request, From, State) ->
    erlang:error(unexpected_call, [Request, From, State]).

handle_cast({set_timer, Time, Pid, NewMessage}, #state{message = Message} = State) when Message =:= {} ->
    {noreply, State#state{time = Time, pid = Pid, message = NewMessage}};

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
