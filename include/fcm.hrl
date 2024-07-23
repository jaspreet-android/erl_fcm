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