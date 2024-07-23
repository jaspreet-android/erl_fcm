-module(fcm_wpool_rdbms).
-author("jaspreet.android@gmail.com").
-behaviour(fcm_wpool).

-export([init/0]).
-export([start/4]).
-export([stop/2]).

%% --------------------------------------------------------------
%% fcm_wpool callbacks
-spec init() -> ok.
init() ->
    fcm_rdbms:create_ets_table(
      prepared_statements, [named_table, public, {read_concurrency, true}]).

-spec start(mongooseim:host_type_or_global(), fcm_wpool:tag(),
            fcm_wpool:pool_opts(), fcm_wpool:conn_opts()) -> {ok, pid()} | {error, any()}.
start(HostType, Tag, WpoolOpts, RdbmsOpts) ->
    try do_start(HostType, Tag, WpoolOpts, RdbmsOpts)
    catch
        Err -> {error, Err}
    end.

-spec stop(mongooseim:host_type_or_global(), fcm_wpool:tag()) -> ok.
stop(_, _) ->
    ok.

%% --------------------------------------------------------------
%% Helper functions
do_start(HostType, Tag, WpoolOpts0, RdbmsOpts) when is_list(WpoolOpts0), is_map(RdbmsOpts) ->
    #{driver := BackendName} = RdbmsOpts,
    fcm_backend:init(global, fcm_rdbms, [query, execute], #{backend => BackendName}),
    WpoolOpts = make_wpool_opts(WpoolOpts0, RdbmsOpts),
    ProcName = fcm_wpool:make_pool_name(rdbms, HostType, Tag),
    fcm_wpool:start_sup_pool(rdbms, ProcName, WpoolOpts).

make_wpool_opts(WpoolOpts0, RdbmsOpts) ->
    Worker = {fcm_rdbms, RdbmsOpts},
    [{worker, Worker}, {pool_sup_shutdown, infinity} | WpoolOpts0].
