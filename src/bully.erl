-module(bully).

-behaviour(gen_server).

%% API
-export([start_link/1, stop/0, cluster_event/1, get_state/0]).

%% gen_server callbacks
-export([init/1,
         handle_cast/2,
         handle_info/2,
         handle_call/3,
         terminate/2,
         code_change/3]).

%% State record and definitions of all possible state names
-include("state.hrl").

%% ===================================================================
%% Module API
%% ===================================================================

start_link(State) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

stop() ->
    gen_server:cast(?MODULE, stop).

-spec(cluster_event(term()) -> ok).
cluster_event({ping, From}) ->
    gen_server:cast(?MODULE, {ping, From}),
    ok;
cluster_event({pong, From}) ->
    gen_server:cast(?MODULE, {pong, From}),
    ok;
cluster_event(Event) ->
    lager:error("Unknown remote event ~p on node ~p", [Event, node()]),
    ok.

-spec(get_state() -> term()).
get_state() ->
    {state, State} = gen_server:call(?MODULE, get_state),
    State.

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init(#state{leader = Leader} = State) ->
    Name = case node() of
               Leader ->
                   lager:info("Start leading on ~p", [node()]),
                   ?LEADING;
               _ ->
                   gen_server:cast(?MODULE, ping_leader),
                   ?PINGED
           end,
    {ok, State#state{name = Name}}.

handle_cast(ping_leader, #state{name = Name} = State) when Name =:= ?PINGED ->
    lager:info("Ping leader ~p", [State#state.leader]),
    multicast([State#state.leader], {ping, node()}),
    send_after(State#state.timeout * 4, ping_leader),
    NewState = State#state{name = ?PINGING},
    {noreply, NewState};

handle_cast(ping_leader, #state{name = Name} = State) when Name =:= ?PINGING ->
    lager:info("Timeout: no pong from leader ~p", [State#state.leader]),
    lager:info("Ping leader ~p", [State#state.leader]),
    multicast([State#state.leader], {ping, node()}),
    send_after(State#state.timeout * 4, ping_leader),
    {noreply, State};

handle_cast({ping, From}, #state{name = Name} = State) when Name =:= ?LEADING ->
    lager:info("Ping from ~p", [From]),
    multicast([From], {pong, node()}),
    {noreply, State};

handle_cast({pong, From}, #state{name = Name} = State) when Name =:= ?PINGING ->
    lager:info("Pong from leader ~p", [From]),
    NewState = State#state{name = ?PINGED},
    {noreply, NewState};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Event, #state{name = Name} = State) ->
    lager:error("Unexpected event ~p in ~p", [Event, Name]),
    {noreply, State}.

handle_info({timer, Event}, State) ->
    gen_server:cast(?MODULE, Event),
    {noreply, State};

handle_info(Request, State) ->
    lager:error("Unexpected info ~p", [Request]),
    {noreply, State}.

handle_call(get_state, _From, State) ->
    {reply, {state, State}, State};

handle_call(Request, From, State) ->
    lager:error("Unexpected call ~p from ~p", [Request, From]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

send_after(Timeout, Event) ->
    timer:send_after(Timeout, ?MODULE, {timer, Event}),
    ok.

multicast(Nodes, Event) ->
    [rpc:cast(Node, bully, cluster_event, [Event]) || Node <- Nodes],
    ok.
