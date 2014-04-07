-module(bully_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Module API
%% ===================================================================

start() ->
    lager:start(),
    ok = application:start(bully).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    bully_sup:start_link().

stop(_State) ->
    ok = application:stop(lager),
    ok = application:stop(leader).
