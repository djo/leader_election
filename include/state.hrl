-record(state, {
        name :: atom(),
        leader :: atom(),
        node :: atom(),
        nodes :: [atom()],
        timeout :: integer()
}).

%% state names
-define(PINGING, pinging).
-define(PINGED, pinged).
-define(LEADING, leading).
-define(ANNOUNCING, announcing).
-define(ELECTING, electing).
