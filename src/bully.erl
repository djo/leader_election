-module(bully).

-behaviour(gen_server).

%% API
-export([start_link/0]).

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

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    {ok, Timeout} = application:get_env(bully, timeout),
    {ok, #state{timeout = Timeout}}.

handle_cast(Request, State) ->
    lager:error("Unexpected cast ~p", [Request]),
    {noreply, State}.

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
