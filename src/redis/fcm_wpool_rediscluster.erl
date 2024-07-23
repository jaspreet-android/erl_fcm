-module(fcm_wpool_rediscluster).
-author("jaspreet.android@gmail.com").
-behaviour(fcm_wpool).

-export([init/0]).
-export([start/4]).
-export([stop/2]).
-export([is_supported_strategy/1]).
-include("fcm.hrl").
-include("fcm_logger.hrl").
init() ->
  ok.

start(_Host, _Tag, WpoolOptsIn, ConnOpts) ->
  wpool_spec(WpoolOptsIn, ConnOpts).

stop(_, _) ->
  ok.

is_supported_strategy(available_worker) -> false;
is_supported_strategy(_) -> true.

%%%===================================================================
%%% Internal functions
%%%===================================================================

wpool_spec(WpoolOptsIn, RedisOpts) ->
  HostsList = maps:get(hosts, RedisOpts, [#{"127.0.0.1" => 6379}]),
  Hosts = [{maps:get(ip_address,Host),maps:get(port,Host)} || Host <- HostsList],
  PoolSize = maps:get(pool_size, RedisOpts, 5),
  PoolMaxOverflow = maps:get(pool_max_overflow, RedisOpts, 10),
  Password = maps:get(password, RedisOpts, ""),
  SendTimeout = maps:get(send_timeout, RedisOpts, 6000),
  ?LOG_WARNING(#{what => wpool_spec, hosts => Hosts, poolsize => PoolSize, max_over => PoolMaxOverflow, passowrd => Password, send_timeout => SendTimeout}),
  application:set_env(eredis_cluster, pool_size, PoolSize),
  application:set_env(eredis_cluster, pool_max_overflow, PoolMaxOverflow),
  application:set_env(eredis_cluster, password, Password),
  application:set_env(eredis_cluster, socket_options, [{send_timeout, SendTimeout}]),
  eredis_cluster:connect(Hosts),
  WpoolOptsIn.