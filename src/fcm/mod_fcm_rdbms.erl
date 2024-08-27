%%%-------------------------------------------------------------------
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Jul 2024 3:58 pm
%%%-------------------------------------------------------------------
-module(mod_fcm_rdbms).
-author("rakshit").

-include("fcm.hrl").

-export([prepare_queries/0, addFcmToken/6, updateFcmToken/3,
         selectFcmToken/1, deleteFcmToken/1, start/0, selectToken/1]).

-import(fcm_rdbms, [prepare/4, execute_successfully/3]).
-import(maps, [ get/3 ]).
-import(fcm_app, [ env/0]).

start() ->
  prepare_queries().

prepare_queries() ->
  prepare(add_fcm_token, fcm_tokens, [user_id, fcm_token, device_type, token, resource, luser],
    <<"INSERT INTO fcm_tokens (user_id, fcm_token, device_type, token, resource, luser) "
    "VALUES (?, ?, ?, ?, ?, ?) ON CONFLICT (luser) DO UPDATE SET "
    "fcm_token = EXCLUDED.fcm_token;">>),

  prepare(delete_fcm_token, fcm_tokens, [fcm_token],
    <<"DELETE FROM fcm_tokens
    WHERE fcm_token = ? ">>),

  prepare(update_fcm_token,fcm_tokens,[fcm_token, token, luser],
    <<"UPDATE fcm_tokens SET fcm_token = ? , token = ?
    WHERE luser = ? ">>),

  prepare(select_fcm_token, fcm_tokens, [user_id],
    <<"SELECT user_id, fcm_token, device_type, token FROM fcm_tokens
    WHERE luser = ? ">>),

  prepare(fcm_token, fcm_tokens, [user_id],
    <<"SELECT fcm_token, device_type FROM fcm_tokens
    WHERE luser = ? ">>).
gethost() ->
 get(host, env(), <<"localhost">>).

addFcmToken(UserId, FcmToken, DeviceType, Token, Resource, LUser) ->
  case execute_successfully(gethost(), add_fcm_token, [UserId, FcmToken, DeviceType, Token, Resource, LUser]) of
    {updated, _} ->
      ok;
    _ ->
      error
  end.

updateFcmToken(FcmToken, Token, LUser) ->
  case execute_successfully(gethost(), update_fcm_token, [FcmToken, Token, LUser]) of
    {updated, _} ->
      ok;
    {error, _} ->
      error
  end.

deleteFcmToken(FcmToken) ->
  case execute_successfully(gethost(), delete_fcm_token, [FcmToken]) of
    {updated, _} ->
      ok;
    {error, _} ->
      error
  end.

selectFcmToken(LUser) ->
  case execute_successfully(gethost(), select_fcm_token, [LUser]) of
    {selected, Data} ->
      {ok, Data};
    {error, _} ->
      error;
    _ ->
      {ok, []}
  end.

selectToken(LUser) ->
  case execute_successfully(gethost(), fcm_token, [LUser]) of
    {selected, Data} ->
      {ok, Data};
    {error, _} ->
      error
  end.