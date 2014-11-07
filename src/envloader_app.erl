-module(envloader_app).
-behaviour(application).

%% Application Callbacks
-export([start/2, stop/1]).

%% ====================================================================
%% API Functions
%% ====================================================================

start(_StartType, _StartArgs) -> envloader_sup:start_link().

stop(_State) -> ok.

%% ====================================================================
%% Internal Functions
%% ====================================================================
