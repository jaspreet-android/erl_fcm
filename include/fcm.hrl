-define(LOG_FORMAT(Fmt), "[~s.~w]: " ++ Fmt).

-define(LOG_DATA(), ?LOG_DATA([])).
-define(LOG_DATA(Data), [?MODULE, ?LINE] ++ Data).

-define(INFO(Fmt), ?INFO(Fmt, [])).
-define(INFO(Fmt, Data), error_logger:info_msg(?LOG_FORMAT(Fmt), ?LOG_DATA(Data))).

-define(WARN(Fmt), ?WARN(Fmt, [])).
-define(WARN(Fmt, Data), error_logger:warning_msg(?LOG_FORMAT(Fmt), ?LOG_DATA(Data
))).

-define(ERR(Fmt), ?ERR(Fmt, [])).
-define(ERR(Fmt, Data), error_logger:error_msg(?LOG_FORMAT(Fmt), ?LOG_DATA(Data))
).

-define(SEQ(N), lists:seq(1, N)).
-define(SEQ(N, St), [St || _ <- ?SEQ(N)]).

-define(MYHOSTS, [<<"localhost">>]).
-type proplist() :: list(tuple()).

-define(CTYPE_KEY, <<"Content-Type">>).
-define(JSON_MIME, <<"application/x-json">>).

%% FCM PUSH
-define(GRANT_TYPE, <<"urn:ietf:params:oauth:grant-type:jwt-bearer">>).
-define(JWT_SCOPE, <<"https://www.googleapis.com/auth/firebase.messaging">>).
-define(JWT_AUDIENCE, <<"https://oauth2.googleapis.com/token">>).
-define(FCM_API_URL, "https://fcm.googleapis.com/v1/projects/hookzapp/messages:send").
-define(FCM_AUTH_FILENAME, "serviceAccountKey.json").