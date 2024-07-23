-module(rdbms_queries).
-author("jaspreet.android@gmail.com").

-export([get_db_type/0,
         begin_trans/0,
         get_db_specific_limits/0,
         get_db_specific_limits/1,
         get_db_specific_limits_binaries/0,
         get_db_specific_limits_binaries/1,
         get_db_specific_offset/2,
         add_limit_arg/2,
         limit_offset_sql/0,
         limit_offset_args/2,
         sql_transaction/2,
         count_records_where/3,
         create_bulk_insert_query/3]).

-export([join/2,
         prepare_upsert/6,
         prepare_upsert/7,
         execute_upsert/5,
         request_upsert/5]).

-ignore_xref([
    count_records_where/3, get_db_specific_limits/1, get_db_specific_offset/2, get_db_type/0
]).

%% We have only two compile time options for db queries:
%%-define(generic, true).
%%-define(mssql, true).
-ifndef(mssql).
-undef(generic).
-endif.

-define(RDBMS_TYPE, (mongoose_rdbms:db_type())).

-ifdef(gen_server_request_id).
-type request_id() :: gen_server:request_id().
-else.
-type request_id() :: term().
-endif.

%
%% -----------------
%% Common functions

join([], _Sep) ->
    [];
join([H|T], Sep) ->
    [H, [[Sep, X] || X <- T]].

%% -----------------
%% Generic queries

get_db_type() ->
    ?RDBMS_TYPE.

-spec execute_upsert(Host :: mongoose_rdbms:server(),
                     Name :: atom(),
                     InsertParams :: [any()],
                     UpdateParams :: [any()],
                     UniqueKeyValues :: [any()]) -> mongoose_rdbms:query_result().
execute_upsert(Host, Name, InsertParams, UpdateParams, UniqueKeyValues) ->
    case {mongoose_rdbms:db_engine(Host), mongoose_rdbms:db_type()} of
        {mysql, _} ->
            mongoose_rdbms:execute(Host, Name, InsertParams ++ UpdateParams);
        {pgsql, _} ->
            mongoose_rdbms:execute(Host, Name, InsertParams ++ UpdateParams);
        {odbc, mssql} ->
            mongoose_rdbms:execute(Host, Name, UniqueKeyValues ++ InsertParams ++ UpdateParams);
        NotSupported -> erlang:error({rdbms_not_supported, NotSupported})
    end.

-spec request_upsert(Host :: mongoose_rdbms:server(),
                     Name :: atom(),
                     InsertParams :: [any()],
                     UpdateParams :: [any()],
                     UniqueKeyValues :: [any()]) -> request_id().
request_upsert(Host, Name, InsertParams, UpdateParams, UniqueKeyValues) ->
    case {mongoose_rdbms:db_engine(Host), mongoose_rdbms:db_type()} of
        {mysql, _} ->
            mongoose_rdbms:execute_request(Host, Name, InsertParams ++ UpdateParams);
        {pgsql, _} ->
            mongoose_rdbms:execute_request(Host, Name, InsertParams ++ UpdateParams);
        {odbc, mssql} ->
            mongoose_rdbms:execute_request(Host, Name, UniqueKeyValues ++ InsertParams ++ UpdateParams);
        NotSupported -> erlang:error({rdbms_not_supported, NotSupported})
    end.

%% @doc
%% This functions prepares query for inserting a row or updating it if already exists
%% Updates can be either fields or literal expressions like "column = tab.column + 1"
-spec prepare_upsert(Host :: mongoose_rdbms:server(),
                     QueryName :: atom(),
                     TableName :: atom(),
                     InsertFields :: [binary()],
                     Updates :: [binary() | {assignment | expression, binary(), binary()}],
                     UniqueKeyFields :: [binary()]) ->
    {ok, QueryName :: atom()} | {error, already_exists}.
prepare_upsert(Host, Name, Table, InsertFields, Updates, UniqueKeyFields) ->
    prepare_upsert(Host, Name, Table, InsertFields, Updates, UniqueKeyFields, none).

-spec prepare_upsert(Host :: mongoose_rdbms:server(),
                     QueryName :: atom(),
                     TableName :: atom(),
                     InsertFields :: [ColumnName :: binary()],
                     Updates :: [binary() | {assignment | expression, binary(), binary()}],
                     UniqueKeyFields :: [binary()],
                     IncrementalField :: none | binary()) ->
    {ok, QueryName :: atom()} | {error, already_exists}.
prepare_upsert(Host, Name, Table, InsertFields, Updates, UniqueKeyFields, IncrementalField) ->
    SQL = upsert_query(Host, Table, InsertFields, Updates, UniqueKeyFields, IncrementalField),
    Query = iolist_to_binary(SQL),
    Fields = prepared_upsert_fields(InsertFields, Updates, UniqueKeyFields),
    mongoose_rdbms:prepare(Name, Table, Fields, Query).

prepared_upsert_fields(InsertFields, Updates, UniqueKeyFields) ->
    UpdateFields = lists:filtermap(fun get_field_name/1, Updates),
    case mongoose_rdbms:db_type() of
        mssql ->
            UniqueKeyFields ++ InsertFields ++ UpdateFields;
        _ -> InsertFields ++ UpdateFields
    end.

upsert_query(Host, Table, InsertFields, Updates, UniqueKeyFields, IncrementalField) ->
    case {mongoose_rdbms:db_engine(Host), mongoose_rdbms:db_type()} of
        {mysql, _} ->
            upsert_mysql_query(Table, InsertFields, Updates, UniqueKeyFields, IncrementalField);
        {pgsql, _} ->
            upsert_pgsql_query(Table, InsertFields, Updates, UniqueKeyFields, IncrementalField);
        {odbc, mssql} ->
            upsert_mssql_query(Table, InsertFields, Updates, UniqueKeyFields);
        NotSupported -> erlang:error({rdbms_not_supported, NotSupported})
    end.

mysql_and_pgsql_insert(Table, Columns) ->
    JoinedFields = join(Columns, <<", ">>),
    Placeholders = lists:duplicate(length(Columns), $?),
    ["INSERT INTO ", atom_to_binary(Table, utf8), " (",
     JoinedFields,
     ") VALUES (",
     join(Placeholders, ", "),
     ")"].

upsert_mysql_query(Table, InsertFields, Updates, [Key | _], IncrementalField) ->
    Insert = mysql_and_pgsql_insert(Table, InsertFields),
    OnConflict = mysql_on_conflict(Table, Updates, Key, IncrementalField),
    [Insert, OnConflict].

upsert_pgsql_query(Table, InsertFields, Updates, UniqueKeyFields, IncrementalField) ->
    Insert = mysql_and_pgsql_insert(Table, InsertFields),
    OnConflict = pgsql_on_conflict(Updates, UniqueKeyFields),
    WhereIncrements = pgsql_ensure_increments(Table, IncrementalField),
    [Insert, OnConflict, WhereIncrements].

mysql_on_conflict(_Table, [], Key, _) ->
    %% Update field to itself (no-op), there is no 'DO NOTHING' in MySQL
    [" ON DUPLICATE KEY UPDATE ", Key, " = ", Key];
mysql_on_conflict(_Table, UpdateFields, _, none) ->
    [" ON DUPLICATE KEY UPDATE ",
     update_fields_on_conflict(UpdateFields)];
mysql_on_conflict(Table, UpdateFields, _, IncrementalField) ->
    TableName = atom_to_list(Table),
    FieldsWithPlaceHolders = [mysql_fields_with_placeholders(TableName, Update, IncrementalField)
                              || Update <- UpdateFields],
    IncrUpdates = join(FieldsWithPlaceHolders, ", "),
    [" AS alias ON DUPLICATE KEY UPDATE ", IncrUpdates].

mysql_fields_with_placeholders(TableName, UpdateField, IncrementalField) ->
    Alternatives = case UpdateField of
                       {Op, Column, Expression} when Op =:= assignment; Op =:= expression ->
                           [Expression, ", ", TableName, ".", Column, ")"];
                       Column ->
                           ["? , ", TableName, ".", Column, ")"]
                   end,
    [ Column, " = IF(", TableName, ".", IncrementalField, " < alias.", IncrementalField, ", "
      | Alternatives].

pgsql_on_conflict([], UniqueKeyFields) ->
    JoinedKeys = join(UniqueKeyFields, ", "),
    [" ON CONFLICT (", JoinedKeys, ") DO NOTHING"];
pgsql_on_conflict(UpdateFields, UniqueKeyFields) ->
    JoinedKeys = join(UniqueKeyFields, ", "),
    [" ON CONFLICT (", JoinedKeys, ")"
     " DO UPDATE SET ",
     update_fields_on_conflict(UpdateFields)].

update_fields_on_conflict(Updates) ->
    FieldsWithPlaceHolders = [get_field_expression(Update) || Update <- Updates],
    join(FieldsWithPlaceHolders, ", ").

pgsql_ensure_increments(_Table, none) ->
    [];
pgsql_ensure_increments(Table, IncrementalField) ->
    TableName = atom_to_list(Table),
    [" WHERE ", TableName, ".", IncrementalField, " < EXCLUDED.", IncrementalField].

upsert_mssql_query(Table, InsertFields, Updates, UniqueKeyFields) ->
    UniqueKeysInSelect = [[" ? AS ", Key] || Key <- UniqueKeyFields],
    BinTab = atom_to_binary(Table, utf8),
    UniqueConstraint = [[BinTab, ".", Key, " = source.", Key] || Key <- UniqueKeyFields],
    JoinedInsertFields = join(InsertFields, ", "),
    Placeholders = lists:duplicate(length(InsertFields), $?),
    ["MERGE INTO ", BinTab, " WITH (SERIALIZABLE)"
     " USING (SELECT ", join(UniqueKeysInSelect, ", "), ")"
            " AS source (", join(UniqueKeyFields, ", "), ")"
        " ON (", join(UniqueConstraint, " AND "), ")"
     " WHEN NOT MATCHED THEN INSERT"
       " (", JoinedInsertFields, ")"
         " VALUES (", join(Placeholders, ", "), ")" | mssql_on_conflict(Updates)].

mssql_on_conflict([]) -> ";";
mssql_on_conflict(Updates) ->
     [" WHEN MATCHED THEN UPDATE SET ", update_fields_on_conflict(Updates), ";"].

get_field_expression({Op, ColumnName, Expr}) when Op =:= assignment; Op =:= expression ->
    [ColumnName, " = ", Expr];
get_field_expression(Field) when is_binary(Field) ->
    [Field, " = ?"].

get_field_name({assignment, Field, _}) when is_binary(Field) ->
    false;
get_field_name({expression, Field, _}) when is_binary(Field) ->
    {true, Field};
get_field_name(Field) when is_binary(Field) ->
    true.

%% F can be either a fun or a list of queries
%% TODO: We should probably move the list of queries transaction
%% wrapper from the mongoose_rdbms module to this one (rdbms_queries)
sql_transaction(LServer, F) ->
    mongoose_rdbms:sql_transaction(LServer, F).

begin_trans() ->
    begin_trans(?RDBMS_TYPE).

begin_trans(mssql) ->
    rdbms_queries_mssql:begin_trans();
begin_trans(_) ->
    [<<"BEGIN;">>].

%% Count number of records in a table given a where clause
count_records_where(LServer, Table, WhereClause) ->
    mongoose_rdbms:sql_query(
      LServer,
      [<<"select count(*) from ">>, Table, " ", WhereClause, ";"]).

-spec create_bulk_insert_query(Table :: iodata() | atom(), Fields :: [iodata() | atom()],
                               RowsNum :: pos_integer()) ->
    {iodata(), [binary()]}.
create_bulk_insert_query(Table, Fields, RowsNum) when is_atom(Table) ->
    create_bulk_insert_query(atom_to_binary(Table, utf8), Fields, RowsNum);
create_bulk_insert_query(Table, [Field | _] = Fields, RowsNum) when is_atom(Field) ->
    create_bulk_insert_query(Table, [atom_to_binary(F, utf8) || F <- Fields], RowsNum);
create_bulk_insert_query(Table, Fields, RowsNum) when RowsNum > 0 ->
    JoinedFields = join(Fields, <<", ">>),
    Placeholders = lists:duplicate(length(Fields), <<"?">>),
    PlaceholderSet = [<<"(">>, join(Placeholders, <<", ">>), <<")">>],
    PlaceholderSets = lists:duplicate(RowsNum, PlaceholderSet),
    JoinedPlaceholderSets = join(PlaceholderSets, <<", ">>),
    Sql = [<<"INSERT INTO ">>, Table, <<" (">>, JoinedFields, <<") "
       "VALUES ">>, JoinedPlaceholderSets, <<";">>],
    Fields2 = lists:append(lists:duplicate(RowsNum, Fields)),
    {Sql, Fields2}.

get_db_specific_limits() ->
    do_get_db_specific_limits(?RDBMS_TYPE, "?", true).

get_db_specific_limits_binaries() ->
    {LimitSQL, LimitMSSQL} = get_db_specific_limits(),
    {list_to_binary(LimitSQL), list_to_binary(LimitMSSQL)}.

-spec get_db_specific_limits(integer())
        -> {SQL :: nonempty_string(), []} | {[], MSSQL::nonempty_string()}.
get_db_specific_limits(Limit) ->
    LimitStr = integer_to_list(Limit),
    do_get_db_specific_limits(?RDBMS_TYPE, LimitStr, false).

-spec get_db_specific_offset(integer(), integer()) -> iolist().
get_db_specific_offset(Offset, Limit) ->
    do_get_db_specific_offset(?RDBMS_TYPE, integer_to_list(Offset), integer_to_list(Limit)).


%% Arguments:
%% - Type (atom) - database type
%% - LimitStr (string) - a field value
%% - Wrap (boolean) - add parentheses around a field for MSSQL
do_get_db_specific_limits(mssql, LimitStr, _Wrap = false) ->
    {"", "TOP " ++ LimitStr};
do_get_db_specific_limits(mssql, LimitStr, _Wrap = true) ->
    {"", "TOP (" ++ LimitStr ++ ")"};
do_get_db_specific_limits(_, LimitStr, _Wrap) ->
    {"LIMIT " ++ LimitStr, ""}.

do_get_db_specific_offset(mssql, Offset, Limit) ->
    [" OFFSET ", Offset, " ROWS"
    " FETCH NEXT ", Limit, " ROWS ONLY"];
do_get_db_specific_offset(_, Offset, _Limit) ->
    [" OFFSET ", Offset].

add_limit_arg(Limit, Args) ->
    add_limit_arg(?RDBMS_TYPE, Limit, Args).

add_limit_arg(mssql, Limit, Args) ->
    [Limit|Args];
add_limit_arg(_, Limit, Args) ->
    Args ++ [Limit].

get_db_specific_limits_binaries(Limit) ->
    {LimitSQL, LimitMSSQL} = get_db_specific_limits(Limit),
    {list_to_binary(LimitSQL), list_to_binary(LimitMSSQL)}.

limit_offset_sql() ->
    limit_offset_sql(?RDBMS_TYPE).

limit_offset_sql(mssql) ->
    <<" OFFSET (?) ROWS FETCH NEXT (?) ROWS ONLY">>;
limit_offset_sql(_) ->
    <<" LIMIT ? OFFSET ?">>.

limit_offset_args(Limit, Offset) ->
    limit_offset_args(?RDBMS_TYPE, Limit, Offset).

limit_offset_args(mssql, Limit, Offset) -> [Offset, Limit];
limit_offset_args(_, Limit, Offset) -> [Limit, Offset].
