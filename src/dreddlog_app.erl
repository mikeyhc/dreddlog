-module(dreddlog_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% Application callbacks
start(_StartType, []) -> dreddlog_sup:start_link().
stop(_State) -> ok.
