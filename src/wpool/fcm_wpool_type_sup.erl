-module(fcm_wpool_type_sup).
-author("jaspreet.android@gmail.com").
-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([name/1]).

%% Supervisor callbacks
-export([init/1]).

-ignore_xref([start_link/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(fcm_wpool:pool_type()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(PoolType) ->
    supervisor:start_link({local, name(PoolType)}, ?MODULE, [PoolType]).

-spec name(fcm_wpool:pool_type()) -> mongoose_wpool:proc_name().
name(PoolType) ->
    list_to_atom("mongoose_wpool_" ++ atom_to_list(PoolType) ++ "_sup").

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
                                    [#{id := mongoose_wpool:proc_name(),
                                       start := {mongoose_wpool_mgr, start_link, [mongoose_wpool:pool_type()]},
                                       restart => transient,
                                       shutdown => brutal_kill,
                                       type => worker,
                                       modules => [module()]}]}}.
init([PoolType]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 100,
                 period => 5},

    ChildSpec = #{id => fcm_wpool_mgr:name(PoolType),
                  start => {fcm_wpool_mgr, start_link, [PoolType]},
                  restart => transient,
                  shutdown => brutal_kill,
                  type => worker,
                  modules => [?MODULE]},

    {ok, {SupFlags, [ChildSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
