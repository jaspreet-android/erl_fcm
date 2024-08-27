-module(mod_fcm_push_handler).

-include("fcm.hrl").

-export([send_push/3]).

-import(fcm_app, [ env/0]).
-import(proplists, [ get_value/2 ]).
-import(maps, [ get/2 ]).
-import(mod_fcm_utils, [generate_access_token/1, build_ios_payload/2, build_android_payload/2]).


send_push(Message, RegistrationToken, DeviceType) ->
  MessageInMap = maps:from_list(Message),
  send_notification(RegistrationToken, MessageInMap, DeviceType).

send_notification(RegistrationToken, Message, DeviceType) ->
  AccessToken = get_firebase_access_token(),
  Payload = case DeviceType of
              <<"ios">> ->
                build_ios_payload(RegistrationToken, Message);
              <<"android">> ->
                build_android_payload(RegistrationToken, Message)
            end,
  send_fcm_request(AccessToken, Payload).

get_firebase_access_token() ->
  Filename = filename:join([code:priv_dir(erl_fcm), "config", ?FCM_AUTH_FILENAME]),
  {ok, ServiceAccountJson} = file:read_file(Filename),
  Key = jsx:decode(ServiceAccountJson),
  generate_access_token(Key).

send_fcm_request(AccessToken, Payload) ->
  inets:start(),
  URL = ?FCM_API_URL,
  Headers = [
    {"Content-Type", "application/json"},
    {"Authorization", "Bearer " ++ binary_to_list(AccessToken)}
  ],
  {ok,  {{_, ResCode, _}, _, Reason}} = httpc:request(post, {URL, Headers, "application/json", Payload}, [], []),
  io:format("Reason ~p~n", [Reason]), %% TODO remove after testing
  handle_response(ResCode, Reason).

handle_response(ResCode, Reason) ->
 case ResCode of
   200 -> {ok, <<"Notification sent">>};
   _ -> {error, Reason}
 end.