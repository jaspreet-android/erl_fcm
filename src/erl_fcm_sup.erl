%%%-------------------------------------------------------------------
%% @doc erl_fcm top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erl_fcm_sup).
-author("jaspreet.android@gmail.com").
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  ChildsSpec = [
    fcm_app:child_spec()
  ],
  {ok, {{one_for_one, 10, 60}, ChildsSpec}}.

