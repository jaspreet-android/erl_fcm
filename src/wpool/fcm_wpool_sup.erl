-module(fcm_wpool_sup).
-author("jaspreet.android@gmail.com").
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-export([child_spec/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2, 3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, {#{strategy => one_for_one, intensity => 100, period => 5},
                                    []}}.
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 100,
                 period => 5},

    {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec child_spec(fcm_wpool:pool_type()) ->
    #{id := fcm_wpool:proc_name(),
      start := {fcm_wpool_type_sup, start_link, [fcm_wpool:pool_type()]},
      restart => transient,
      shutdown => infinity,
      type => supervisor,
      modules => [module()]}.
child_spec(Type) ->
    #{id => fcm_wpool_type_sup:name(Type),
      start => {fcm_wpool_type_sup, start_link, [Type]},
      restart => transient,
      shutdown => infinity,
      type => supervisor,
      modules => [fcm_wpool_type_sup]
     }.

