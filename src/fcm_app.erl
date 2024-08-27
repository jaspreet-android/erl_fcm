%%%-------------------------------------------------------------------
%%% @author rakshit
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jul 2024 12:17 pm
%%%-------------------------------------------------------------------
-module(fcm_app).

%%====================================================================
%% EXPORTS
%%====================================================================
-export([
   stop/0
  , env/0
  , child_spec/0]).

%%====================================================================
%% IMPORTS
%%====================================================================
-import(proplists, [ get_value/3 ]).
-import(maps, [ get/3 ]).

%%====================================================================
%% RECORDS
%%====================================================================
-record(?MODULE, {
  pid                    :: pid(),
  globals                :: list()
}).

-behaviour(gen_server).

-export([ init/1
  , handle_call/3
  , handle_cast/2
  , handle_info/2
  , terminate/2
  , code_change/3 ]).


%%====================================================================
%%               _     _ _                    _
%%   _ __  _   _| |__ | (_) ___    __ _ _ __ (_)
%%  | '_ \| | | | '_ \| | |/ __|  / _` | '_ \| |
%%  | |_) | |_| | |_) | | | (__  | (_| | |_) | |
%%  | .__/ \__,_|_.__/|_|_|\___|  \__,_| .__/|_|
%%  |_|                                |_|
%%
%%====================================================================
%%--------------------------------------------------------------------
%% @doc stop the application
%%
%% @end
-spec stop() -> 'ok'.
%%--------------------------------------------------------------------
stop() ->
  _ = application:stop(?MODULE),
  ok.
%%--------------------------------------------------------------------
init(_) ->
  mod_fcm_rdbms:start(),
  false = process_flag(trap_exit, true),
  Pid = cowboy(),
  {ok, #?MODULE{pid = Pid}, hibernate}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get_globals}, _From, #?MODULE{globals = Globals} = State) ->
  {reply, Globals, State, hibernate};

handle_call(Request, From, State) ->
  exit({handle_call, Request, From, State}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
  exit({handle_cast, Msg, State}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
  exit({handle_info, Info, State}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_, #?MODULE{}) ->
  ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%====================================================================
%%              _            _                     _
%%   _ __  _ __(_)_   ____ _| |_ ___    __ _ _ __ (_)
%%  | '_ \| '__| \ \ / / _` | __/ _ \  / _` | '_ \| |
%%  | |_) | |  | |\ V / (_| | ||  __/ | (_| | |_) | |
%%  | .__/|_|  |_| \_/ \__,_|\__\___|  \__,_| .__/|_|
%%  |_|                                     |_|
%%
%%====================================================================

-spec cowboy() -> pid().
cowboy() ->
  {ok, Pid} = cowboy(transport()),
  Pid.

-spec cowboy(http | https) -> {ok, pid()}.
cowboy(http) ->
  cowboy:start_clear(http, cowboy_opts(), cowboy_dispatch());
cowboy(https) ->
  cowboy:start_clear(https, cowboy_opts() , cowboy_dispatch()).


%%====================================================================
%%
%%   _       _                        _               _
%%  (_)_ __ | |_ ___ _ __ _ __   __ _| |   __ _ _ __ (_)
%%  | | '_ \| __/ _ \ '__| '_ \ / _` | |  / _` | '_ \| |
%%  | | | | | ||  __/ |  | | | | (_| | | | (_| | |_) | |
%%  |_|_| |_|\__\___|_|  |_| |_|\__,_|_|  \__,_| .__/|_|
%%                                             |_|
%%
%%====================================================================
-spec child_spec() -> supervisor:child_spec().
child_spec() ->
  {?MODULE,
    {gen_server, start_link, [ {local, ?MODULE}, ?MODULE, [], [] ]},
    transient, 5000, worker, [ ?MODULE ]}.

%%--------------------------------------------------------------------
%% @doc Loads application's settings.
%% @end
%%--------------------------------------------------------------------
env() ->
  application:get_env(erl_fcm, ?MODULE, [ ]).

%%--------------------------------------------------------------------
%% @doc Returns Cowboy's transport.
%% @end
-spec transport() -> http | https.
%%--------------------------------------------------------------------
transport() ->
  get(transport, env(), http).

%%--------------------------------------------------------------------
%% @doc Returns Cowboy's port number.
%% @end
-spec port() -> non_neg_integer().
%%--------------------------------------------------------------------
port() ->
  get(port, env(), 8081).
%%--------------------------------------------------------------------
%% @doc Returns Cowboy's opts.
%% @end
%%--------------------------------------------------------------------
cowboy_opts() ->
  #{socket_opts => [ {port, port()}],
     num_acceptors => workers()
  }.

%%--------------------------------------------------------------------
%% @doc Returns Cowboy's dispatch opts.
%% @end
-spec cowboy_dispatch() -> #{}.
%%--------------------------------------------------------------------
cowboy_dispatch() ->
  #{env => #{dispatch => dispatch()}
    , compress => true
    , timeout => 12000
  }.

%%--------------------------------------------------------------------
%% @doc Returns Cowboy's workers.
%% @end

%%--------------------------------------------------------------------
cow_mime_types() ->
  [ {mimetypes, cow_mimetypes, all} ].

-spec workers() -> non_neg_integer().
%%--------------------------------------------------------------------
workers() ->
  get(workers, env(), 100).

dispatch() ->
  V  = default_dispatch(),
  Res = cowboy_router:compile([V]),
  Res.

-spec default_dispatch() -> term().
default_dispatch() ->
  Trails = trails:trails([mod_fcm_handler,cowboy_swagger_handler]),
  trails:store(Trails),
  {'_',
    [
      {"/fcm/[...]", mod_fcm_handler, []}
      , {"/notify/[...]", mod_fcm_handler, []}
      , {"/api-docs/swagger.json", cowboy_swagger_json_handler,#{}}
      , {"/api-docs/", cowboy_swagger_redirect_handler, {file, "_build/default/lib/cowboy_swagger/priv/swagger/index.html"}}
      , {"/api-docs/[...]", cowboy_static,{dir, "_build/default/lib/cowboy_swagger/priv/swagger", cow_mime_types()}}

    ]}.
%%--------------------------------------------------------------------
%% @doc Return Create form static file
%% end