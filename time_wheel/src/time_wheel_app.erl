%%%-------------------------------------------------------------------
%% @doc time_wheel public API
%% @end
%%%-------------------------------------------------------------------

-module(time_wheel_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    time_wheel_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
