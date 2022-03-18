%%%-------------------------------------------------------------------
%% @doc spl_poc public API
%% @end
%%%-------------------------------------------------------------------

-module(spl_poc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    spl_poc_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
