-module(fcm_redis_cluster).
-behaviour(gen_server).
-author("jaspreet.android@gmail.com").
-include("fcm_logger.hrl").

%% API
-export([cmd_cluster/1, cmd_cluster_async/1]).

-export([start_link/0,init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).


-spec cmd_cluster(iolist()) -> undefined
| binary()
| [binary() | [binary() | integer()] | integer() | {'error', _}]
| integer()
| {'error', _}.
cmd_cluster(Cmd) ->
    Host = <<"localhost">>,
    case ets:lookup(eredis_cluster_monitor, cluster_state) of
        [] ->
            mongoose_metrics:update(Host, redisError, 1),
            ?LOG_ERROR(#{what => cmd_cluster,
                text => <<"eredis_cluster_monitor empty">>}),
            [];
        _ ->
            mongoose_metrics:update(Host, redis, 1),
            try eredis_cluster:q(Cmd) of
                {error, Error} ->
                    mongoose_metrics:update(Host, redisError, 1),
                    ?LOG_ERROR(#{what => cmd_cluster, text => <<"Redis error">>, error => [Error]}),
                    [];
                {ok, Value} -> Value;
                V -> V
            catch _:Reason ->
                mongoose_metrics:update(Host, redisError, 1),
                ?LOG_ERROR(#{what => cmd_cluster, text => <<"Redis error">>, error => [Reason]}),
                %%TODO try changing it to error tuple
                []
            end
    end.

%% TODO use worker pool of size 10
%% async call to redis
cmd_cluster_async(Cmd)->
    try  gen_server:cast(?MODULE, {redis_write_cmd, Cmd})
    catch  _:Reason ->
        ?LOG_ERROR(#{what => cmd_cluster, text => <<"Redis error">>, error => [Reason]}),
        []
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

init([]) ->
    {ok, []}.

handle_call({redis_write_cmd, Command}, _From, State) ->
    Reply = cmd_cluster(Command),
    {reply, Reply, State}.

handle_cast({redis_write_cmd, Command}, State) ->
    Host = <<"localhost">>,
    mongoose_metrics:update(Host, redisAsync, 1),
    cmd_cluster(Command),
    {noreply, State}.

handle_info({redis_write_cmd, _Command}, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
