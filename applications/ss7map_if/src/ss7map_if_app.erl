-module(ss7map_if_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) ->
    ss7map_if_sup:start_link().

stop(_State) ->
    ok.
