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
cluster_event({announce_election, From}) ->
    gen_server:cast(?MODULE, {announce_election, From}),
    ok;
cluster_event({ok, From}) ->
    gen_server:cast(?MODULE, {ok, From}),
    ok;
cluster_event({new_leader, Leader}) ->
    gen_server:cast(?MODULE, {new_leader, Leader}),
    ok;
cluster_event(Event) ->
    lager:error("Unknown cluster event: ~p", [Event]),
    ok.

-spec(get_state() -> term()).
get_state() ->
    {state, State} = gen_server:call(?MODULE, get_state),
    State.

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init(#state{leader = Leader, node = Node} = State) ->
    Name = case Node of
               Leader ->
                   lager:info("Start leading on ~p", [Node]),
                   multicast(State#state.nodes, {new_leader, Node}),
                   ?LEADING;
               _ ->
                   gen_server:cast(?MODULE, ping_leader),
                   ?PINGED
           end,
    {ok, State#state{name = Name}}.

handle_cast(ping_leader, #state{name = Name} = State) when Name =:= ?PINGED ->
    start_pinging(State);

handle_cast(ping_leader, #state{name = Name} = State) when Name =:= ?PINGING ->
    lager:info("Timeout: no pong from leader ~p", [State#state.leader]),
    start_announcing(State);

handle_cast({ping, From}, #state{name = Name} = State) when Name =:= ?LEADING ->
    lager:info("Ping from ~p", [From]),
    multicast([From], {pong, State#state.node}),
    {noreply, State};

handle_cast({pong, From}, #state{name = Name} = State) when Name =:= ?PINGING ->
    lager:info("Pong from leader ~p", [From]),
    {noreply, State#state{name = ?PINGED}};

handle_cast({announce_election, From}, #state{name = Name} = State) when Name =:= ?ANNOUNCING; Name =:= ?ELECTING ->
    multicast([From], {ok, State#state.node}),
    {noreply, State};

handle_cast({announce_election, From}, State) ->
    lager:info("Election announce from ~p", [From]),
    multicast([From], {ok, State#state.node}),
    start_announcing(State);

handle_cast(check_announce, #state{name = Name} = State) when Name =:= ?ANNOUNCING ->
    lager:info("Announce timeout: no one OK received"),
    start_leading(State);

handle_cast({ok, From}, #state{name = Name} = State) when Name =:= ?ANNOUNCING ->
    lager:info("Announce approved from ~p, start electing", [From]),
    send_after(State#state.timeout, check_electing),
    {noreply, State#state{name = ?ELECTING}};

handle_cast(check_electing, #state{name = Name} = State) when Name =:= ?ELECTING ->
    lager:info("Electing timeout, start announcing again"),
    start_announcing(State);

handle_cast({new_leader, Leader}, #state{name = Name} = State) when Name =:= ?LEADING ->
    lager:warning("Collision to set a new leader ~p while ~p", [Leader, Name]),
    announce_election(State, State#state.nodes);

handle_cast({new_leader, Leader}, State) ->
    lager:info("Set new leader ~p", [Leader]),
    start_pinging(State#state{leader = Leader});

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({Event, From}, #state{name = Name} = State) ->
    lager:warning("Skip event ~p from ~p in state ~p", [Event, From, Name]),
    {noreply, State};

handle_cast(Event, #state{name = Name} = State) ->
    lager:warning("Skip event ~p in state ~p", [Event, Name]),
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

multicast(Nodes, Event) ->
    [rpc:cast(Node, bully, cluster_event, [Event]) || Node <- Nodes],
    ok.

send_after(Timeout, Event) ->
    timer:send_after(Timeout, ?MODULE, {timer, Event}),
    ok.

start_pinging(State) ->
    lager:info("Ping leader ~p", [State#state.leader]),
    multicast([State#state.leader], {ping, State#state.node}),
    send_after(State#state.timeout * 4, ping_leader),
    {noreply, State#state{name = ?PINGING}}.

start_leading(#state{node = Node} = State) ->
    lager:info("Start leading on ~p", [Node]),
    multicast(State#state.nodes, {new_leader, Node}),
    {noreply, State#state{name = ?LEADING, leader = Node}}.

start_announcing(#state{node = Node, nodes = Nodes} = State) ->
    Identity = node_identity(Node),
    HigherIdentityNodes = lists:filter(fun(N) -> node_identity(N) > Identity end, Nodes),
    case HigherIdentityNodes of
        [] -> start_leading(State);
        HigherIdentityNodes -> announce_election(State, HigherIdentityNodes)
    end.

announce_election(State, Nodes) ->
    lager:info("Announce election to ~p", [Nodes]),
    multicast(Nodes, {announce_election, State#state.node}),
    send_after(State#state.timeout, check_announce),
    {noreply, State#state{name = ?ANNOUNCING}}.

node_identity(Node) ->
    {Identity, _Host} = string:to_integer(atom_to_list(Node)),
    Identity.
