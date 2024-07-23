-module(fcm_rdbms_pgsql_codec_boolean).
-author("jaspreet.android@gmail.com").
-behaviour(epgsql_codec).

-export([init/2, names/0, encode/3, decode/3, decode_text/3]).

-export_type([data/0]).

-type data() :: boolean() | 0 | 1.

init(_, _) -> [].

names() ->
    [bool].

encode(true, bool, State) -> encode(1, bool, State);
encode(false, bool, State) -> encode(0, bool, State);
encode(1, bool, _) -> <<1:1/big-signed-unit:8>>;
encode(0, bool, _) -> <<0:1/big-signed-unit:8>>.

decode(<<1:1/big-signed-unit:8>>, bool, _) -> true;
decode(<<0:1/big-signed-unit:8>>, bool, _) -> false.

decode_text(V, _, _) -> V.
