-module(fcm_wpool_mgr).
-author("jaspreet.android@gmail.com").
-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([name/1]).
-export([start/5]).
-export([stop/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-ignore_xref([start_link/1]).

-include("fcm_logger.hrl").

-record(state, {type, pools, monitors}).

-type start_request() :: {start_pool,
                          fcm_wpool:scope(), fcm_wpool:tag(),
                          [any()], [any()]}.

-type stop_request() :: {stop_pool,
                            fcm_wpool:scope(), fcm_wpool:tag()}.

-type request() :: start_request() | stop_request().

-type monitored_pool() :: {fcm_wpool:pool_type(), fcm_wpool:scope(), fcm_wpool:tag()}.

-type known_pools() :: #{monitored_pool() := #{monitor := undefined | reference(),
                                               wpool_opts := [wpool:option()],
                                               conn_opts := [term()]}}.

-type state() :: #state{type :: atom(),
                        pools :: known_pools(),
                        monitors :: #{reference() := monitored_pool()}}.
-type reply() :: ok.

%%%===================================================================
%%% API
%%%===================================================================

start_link(Type) ->
    gen_server:start_link({local, name(Type)}, ?MODULE, [Type], []).

start(Type, Host, Tag, PoolOpts, ConnOpts) ->
    ok = ensure_started(Type),
    gen_server:call(name(Type), {start_pool, Host, Tag, PoolOpts, ConnOpts}).

stop(Type, Host, Tag) ->
    gen_server:call(name(Type), {stop_pool, Host, Tag}).

-spec name(fcm_wpool:pool_type()) -> mongoose_wpool:proc_name().
name(Type) ->
    list_to_atom("fcm_wpool_" ++ atom_to_list(Type) ++ "_mgr").
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([any()]) -> {ok, state()}.
init([Type]) ->
    {ok, #state{type = Type, pools = #{}, monitors = #{}}}.

-spec handle_call(request(), {pid(), term()}, state()) ->
    {reply, reply(), state()}.
handle_call({start_pool, Host, Tag, WpoolOpts, ConnOpts}, _From,
            #state{type = Type, pools = Pools, monitors = Monitors} = State) ->
    ?LOG_INFO(#{what => pool_starting, pool_type => Type, tag => Tag, server => Host,
                pool_opts => WpoolOpts}),
    case fcm_wpool:call_start_callback(Type, [Host, Tag, WpoolOpts, ConnOpts]) of
        {_, Pid} = OkReply when is_pid(Pid) ->
            Ref = erlang:monitor(process, Pid),
            Key = {Type, Host, Tag},
            NewMonitors = Monitors#{Ref => Key},
            NewPools = Pools#{Key => #{monitor => Ref,
                                       wpool_opts => WpoolOpts,
                                       conn_opts => ConnOpts}},
            {reply, OkReply, State#state{pools = NewPools, monitors = NewMonitors}};
        Other ->
            ?LOG_ERROR(#{what => pool_start_failed,
                         pool_type => Type, server => Host, reason => Other}),
            {reply, Other, State}
    end;
handle_call({stop_pool, Host, Tag}, _From,
            #state{type = Type, pools = Pools, monitors = Monitors} = State) ->
    Key = {Type, Host, Tag},
    case maps:take(Key, Pools) of
        error ->
            {reply, {error, unknown_pool}, State};
        {Pool, NewPools} ->
            {Result, NewMonitors} = maybe_stop_pool(Key, Pool, Monitors),
            {reply, Result, State#state{pools = NewPools, monitors = NewMonitors}}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', MRef, process, _Pid, Reason}, #state{monitors = Monitors} = State) ->
    case maps:take(MRef, Monitors) of
        error ->
            {noreply, State};
        {Details, NewMonitors0} ->
            NewState = restart_pool(Reason, Details, State#state{monitors = NewMonitors0}),
            {noreply, NewState}
    end;
handle_info({restart, PoolKey}, #state{pools = Pools} = State) ->
    case maps:get(PoolKey, Pools, undefined) of
        undefined -> %% The pool was stopped in the meantime, no need to restart it
            {noreply, State};
        Pool ->
            start_or_schedule_another_restart(PoolKey, Pool, State)
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
ensure_started(Type) ->
    Name = name(Type),
    case whereis(Name) of
        undefined ->
            do_start_type_sup(Type);
        _ ->
            ok
    end.
do_start_type_sup(Type) ->
    ChildSpec = fcm_wpool_sup:child_spec(Type),
    case supervisor:start_child(fcm_wpool_sup, ChildSpec) of
        {ok, _} ->
            ok;
        {error, {already_started, _}} ->
            ok;
        Other ->
            ?LOG_ERROR(#{what => pool_sup_start_failed, pool_type => Type, reason => Other}),
            Other
    end.

start_or_schedule_another_restart(PoolKey, Pool, State) ->
    case try_starting(PoolKey, Pool, State) of
        {ok, NewState} ->
            {noreply, NewState};
        _ ->
            {noreply, do_schedule_restart(PoolKey, Pool, State)}
    end.

try_starting({Type, Host, Tag} = PoolKey,
             #{wpool_opts := WpoolOpts, conn_opts := ConnOpts} = Pool,
             #state{pools = Pools, monitors = Monitors} = State) ->
    case fcm_wpool:call_start_callback(Type, [Host, Tag, WpoolOpts, ConnOpts]) of
        {_, Pid} when is_pid(Pid) ->
            Ref = erlang:monitor(process, Pid),
            NewMonitors = Monitors#{Ref => PoolKey},
            NewPools = Pools#{PoolKey => Pool#{monitor := Ref}},

            {ok, State#state{pools = NewPools, monitors = NewMonitors}};
        Other ->
            ?LOG_WARNING(#{what => pool_restart_failed, pool_key => PoolKey,
                           reason => Other}),
            Other
    end.


restart_pool(Reason, PoolKey, #state{pools = Pools} = State) ->
    ?LOG_ERROR(#{what => pool_restart, pool_key => PoolKey, reason => Reason}),
    case maps:get(PoolKey, Pools, undefined) of
        undefined ->
            ?LOG_WARNING(#{what => restart_unknown_pool_failed,
                           text => <<"Pool failed to restart - pool name is unknown">>,
                           pool_key => PoolKey}),
            State;
        Pool ->
            do_schedule_restart(PoolKey, Pool, State)
    end.

do_schedule_restart(PoolKey, Pool, #state{pools = Pools} = State) ->
    timer:send_after(timer:seconds(2), {restart, PoolKey}),
    NewPool = Pool#{monitor := undefined},
    State#state{pools = Pools#{PoolKey := NewPool}}.

maybe_stop_pool(_, #{monitor := undefined}, Monitors) ->
    {ok, Monitors};
maybe_stop_pool({Type, Host, Tag} = Key, #{monitor := Monitor}, Monitors) ->
    erlang:demonitor(Monitor),
    SupName = fcm_wpool_type_sup:name(Type),
    PoolName = mongoose_wpool:make_pool_name(Type, Host, Tag),
    NewMonitors = maps:remove(Monitor, Monitors),
    case supervisor:terminate_child(SupName, PoolName) of
        ok ->
            {ok, NewMonitors};
        Other ->
            ?LOG_WARNING(#{what => pool_stop_failed, pool_key => Key, reason => Other}),
            {Other, NewMonitors}
    end.
