%%%-------------------------------------------------------------------
%% @doc erl_fcm public API
%% @end
%%%-------------------------------------------------------------------

-module(erl_fcm_app).
-author("jaspreet.android@gmail.com").
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    fcm_wpool:ensure_started(),
    fcm_wpool:start_configured_pools(),
    application:start(epgsql),
    erl_fcm_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
