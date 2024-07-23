-module(fcm_rdbms_mysql).
-author("jaspreet.android@gmail.com").

-type options() :: #{host := string(),
                     port := inet:port_number(),
                     database := string(),
                     username := string(),
                     password := string(),
                     atom() => any()}.

-export([escape_binary/1, unescape_binary/1, connect/2, disconnect/1,
         query/3, prepare/5, execute/4]).

%% API

-spec escape_binary(binary()) -> iodata().
escape_binary(Bin) when is_binary(Bin) ->
    [<<"X'">>, base16:encode(Bin), <<"'">>].

-spec unescape_binary(binary()) -> binary().
unescape_binary(Bin) when is_binary(Bin) ->
    Bin.

-spec connect(options(), QueryTimeout :: non_neg_integer()) ->
                     {ok, Connection :: term()} | {error, Reason :: any()}.
connect(Options, QueryTimeout) ->
   io:format("connect mysql"),
    case mysql:start_link([{query_timeout, QueryTimeout} | db_opts(Options)]) of
        {ok, Ref} ->
            mysql:query(Ref, <<"set names 'utf8mb4';">>),
            mysql:query(Ref, <<"SET SESSION query_cache_type=1;">>),
            {ok, Ref};
        Error ->
            Error
    end.

-spec disconnect(Connection :: term()) -> any().
disconnect(Connection) ->
    gen_server:stop(Connection).

-spec query(Connection :: term(), Query :: any(),
            Timeout :: infinity | non_neg_integer()) -> fcm_rdbms:query_result().
query(Connection, Query, _Timeout) ->
    mysql_to_rdbms(mysql:query(Connection, Query), Connection).

-spec prepare(Connection :: term(), Name :: atom(), Table :: binary(),
              Fields :: [binary()], Statement :: iodata()) ->
                     {ok, term()} | {error, any()}.
prepare(Connection, Name, _Table, _Fields, Statement) ->
    mysql:prepare(Connection, Name, Statement).

-spec execute(Connection :: term(), StatementRef :: term(), Params :: [term()],
              Timeout :: infinity | non_neg_integer()) -> fcm_rdbms:query_result().
execute(Connection, StatementRef, Params, _Timeout) ->
    Params2 = booleans_as_integers(Params),
    mysql_to_rdbms(mysql:execute(Connection, StatementRef, Params2), Connection).

%% Helpers

-spec db_opts(options()) -> [mysql:option()].
db_opts(Options) ->
    FilteredOpts = maps:with([host, port, database, username, password, tls], Options),
    [{found_rows, true} | lists:map(fun process_opt/1, maps:to_list(FilteredOpts))].

process_opt({tls, TLSOpts}) -> {ssl, just_tls:make_ssl_opts(TLSOpts)};
process_opt({username, UserName}) -> {user, UserName};
process_opt(Opt) -> Opt.

%% @doc Convert MySQL query result to Erlang RDBMS result formalism
-spec mysql_to_rdbms(mysql:query_result(), Conn :: term()) -> fcm_rdbms:query_result().
mysql_to_rdbms(ok, Conn) ->
    {updated, mysql:affected_rows(Conn)};
mysql_to_rdbms({ok, _ColumnNames, Rows}, _Conn) ->
    {selected, [list_to_tuple(Row) || Row <- Rows]};
mysql_to_rdbms({ok, Results}, Conn) ->
    [mysql_to_rdbms({ok, Cols, Rows}, Conn) || {Cols, Rows} <- Results];
mysql_to_rdbms({error, {1062, _SQLState, _Message}}, _Conn) ->
    {error, duplicate_key};
mysql_to_rdbms({error, {_Code, _SQLState, Message}}, _Conn) ->
    {error, unicode:characters_to_list(Message)}.

booleans_as_integers([H|T]) when is_boolean(H) ->
    [boolean_to_integer(H)|booleans_as_integers(T)];
booleans_as_integers([H|T]) ->
    [H|booleans_as_integers(T)];
booleans_as_integers([]) ->
    [].

boolean_to_integer(true) -> 1;
boolean_to_integer(false) -> 0.
