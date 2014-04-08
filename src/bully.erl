-module(bully).

-behaviour(gen_server).

%% API
-export([start_link/0, cluster_event/1]).

%% gen_server callbacks
-export([init/1,
         handle_cast/2,
         handle_info/2,
         handle_call/3,
         terminate/2,
         code_change/3]).

-record(state, {
        timeout :: integer()
}).

%% ===================================================================
%% Module API
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

cluster_event({ping, From}) ->
    gen_server:cast(?MODULE, {ping, From}),
    ok;

cluster_event({pong, From}) ->
    gen_server:cast(?MODULE, {pong, From}),
    ok;

cluster_event(Event) ->
    lager:error("Unknown remote event ~p on node ~p", [Event, node()]),
    ok.

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    {ok, Timeout} = application:get_env(bully, timeout),
    gen_server:cast(?MODULE, ping_nodes),
    {ok, #state{timeout = Timeout}}.

handle_cast(ping_nodes, #state{timeout = Timeout} = State) ->
    Nodes = lists:delete(node(), nodes()),
    lager:info("Ping nodes ~p", [Nodes]),
    multicast(Nodes, {ping, node()}),
    send_after(Timeout, ping_nodes),
    {noreply, State};

handle_cast({ping, From}, State) ->
    lager:info("Ping from ~p", [From]),
    multicast([From], {pong, node()}),
    {noreply, State};

handle_cast({pong, From}, State) ->
    lager:info("Pong from ~p", [From]),
    {noreply, State};

handle_cast(Request, State) ->
    lager:error("Unexpected cast ~p", [Request]),
    {noreply, State}.

handle_info({timer, Event}, State) ->
    gen_server:cast(?MODULE, Event),
    {noreply, State};

handle_info(Request, State) ->
    lager:error("Unexpected info ~p", [Request]),
    {noreply, State}.

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
    erlang:send_after(Timeout, ?MODULE, {timer, Event}),
    ok.

multicast(Nodes, Event) ->
    [rpc:cast(Node, bully, cluster_event, [Event]) || Node <- Nodes],
    ok.
