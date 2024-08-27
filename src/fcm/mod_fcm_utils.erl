%%%-------------------------------------------------------------------
%%% @author aniket
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Aug 2024 4:55 PM
%%%-------------------------------------------------------------------
-module(mod_fcm_utils).
-author("aniket").

-include("fcm.hrl").

-compile([export_all]).

build_android_payload(RegistrationToken, Message) ->
  Body = maps:get(<<"body">>, Message),
  Title = maps:get(<<"title">>, Message),
  jsx:encode(#{
    <<"message">> => #{
      <<"notification">> => #{
        <<"body">> => Body,
        <<"title">> => Title
      },
      <<"data">> => Message,
      <<"token">> => RegistrationToken
    }
  }).

build_ios_payload(RegistrationToken, Message) ->
  Body = maps:get(<<"body">>, Message),
  Title = maps:get(<<"title">>, Message),

  Expiration = integer_to_binary(os:system_time(second) + 3600),
  APS = #{
    <<"alert">> => #{
      <<"title">> => get_name(),
      <<"body">> => Body
    },
    <<"sound">> => <<"default">>,
    <<"badge">> => maps:get(<<"count">>, Message, 1),
    <<"category">> => <<"CATEGORY">>,
    <<"mutable-content">> => 1 %% 1 = true, 0 = false
  },
  Payload = #{
    <<"message">> => #{
      <<"token">> => RegistrationToken,
      <<"notification">> => #{
        <<"title">> => get_name(),
        <<"body">> => Body
      },
      <<"data">> => Message,
      <<"apns">> => #{
        <<"headers">> => #{
          <<"apns-push-type">> => <<"alert">>,
          <<"apns-expiration">> => Expiration
        },
        <<"payload">> => #{
          <<"aps">> => APS
        }
      }
    }
  },
  jsx:encode(Payload).

%% JWT
generate_access_token(Key) ->
  TokenUri = proplists:get_value(<<"token_uri">>, Key),
  ClientEmail = proplists:get_value(<<"client_email">>, Key),
  PrivateKey = proplists:get_value(<<"private_key">>, Key),
  JWT = create_jwt(ClientEmail, PrivateKey),
  Body = jsx:encode(#{
    <<"grant_type">> => ?GRANT_TYPE,
    <<"assertion">> => JWT
  }),
  Headers = [{"Content-Type", "application/json"}],
  {ok, {{_, _, _}, _, JsonResponse}} = httpc:request(post, {TokenUri, Headers, "application/json", Body}, [], []),
  {DecodedJson} = jiffy:decode(JsonResponse),
  AccessToken = proplists:get_value(<<"access_token">>, DecodedJson),
  AccessToken.

create_jwt(ClientEmail, PrivateKey) ->
  DatetimeTuple = calendar:universal_time(),
  Timestamp = calendar:datetime_to_gregorian_seconds(DatetimeTuple),
  CurrentTimeInSec = Timestamp - ((calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}) - 1)),
  Claims = #{
    <<"iss">> => ClientEmail,
    <<"scope">> => ?JWT_SCOPE,
    <<"aud">> => ?JWT_AUDIENCE,
    <<"exp">> => CurrentTimeInSec + 3500,
    <<"iat">> => CurrentTimeInSec
  },
  jwerl:sign(Claims, rs256, PrivateKey).

get_name() ->
   maps:get(name, fcm_app:env(), <<"Hookzapp">>).
