-record(state, {
        name :: atom(),
        leader :: atom(),
        nodes :: [atom()],
        timeout :: integer()
}).

%% state names
-define(PINGING, pinging).
-define(PINGED, pinged).
-define(LEADING, leading).
-define(ELECTING, electing).
