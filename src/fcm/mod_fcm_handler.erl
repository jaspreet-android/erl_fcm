%%%-------------------------------------------------------------------
%%% @author jaspreetsingh
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Nov 2022 9:11 AM
%%%-------------------------------------------------------------------
-module(mod_fcm_handler).
-behaviour(trails_handler).

-include("fcm.hrl").

-export([trails/0,init/2]).

%%====================================================================
%% IMPORTS
%%====================================================================
-import(proplists, [get_value/2, get_value/3]).
%%---------------------------------------------------------------
-spec init(term(), term()) -> {'ok', cowboy_req:req(), term()}.
%%--------------------------------------------------------------------
init(Req, State) ->
  handle({cowboy_req:method(Req), Req}, State).
%%-----------------------------------------------

trails() ->
  PostData = #{post =>
  #{tags => ["Data"],
    description => <<"Post all the data">>,
    consumes => ["application/json"],
    produces => ["application/json"],
    requestBody =>  #{
      name => <<"Pages">>,
      in => body,
      description => <<" fcm data to add
        \nExample - {
      \n\"fcm_token\": \"fcm_token1\",
      \n\"device_type\": \"device(Android , IOS , Chrome)\",
      \n\"luser\": \"04c7833f-6845-4a3e-8405-a48017997a53\",
      \n\"resource\": \"phone\",
   }">>,

      required => false,
      content => #{
        'application/json' =>
        #{schema => #{
          type => object,
          properties => #{
            fcm_token => #{type => string},
            device_type => #{type => string},
            luser => #{type => string},
            resource => #{type => string}
          }
        }
        }
      }
    },
    responses => #{
      <<"200">> => #{
        description => <<"">>,
        content => #{
          'application/json' =>
          #{schema => #{
            type => object,
            properties => #{
              success => #{type => boolean},
              total => #{type => integer},
              data => #{type => object}
            }
          }
          }
        }
      }
    }
  }
  },
  PushData = #{post =>
  #{tags => ["Data"],
    description => <<"Post all the data">>,
    consumes => ["application/json"],
    produces => ["application/json"],
    requestBody =>  #{
      name => <<"Pages">>,
      in => body,
      description => <<" fcm data to add
        \nExample - {
      \n\"user_id\": \"id of user\",
   }">>,

      required => false,
      content => #{
        'application/json' =>
        #{schema => #{
          type => object,
          properties => #{
            user_id => #{type => string}
          }
        }
        }
      }
    },
    responses => #{
      <<"200">> => #{
        description => <<"">>,
        content => #{
          'application/json' =>
          #{schema => #{
            type => object,
            properties => #{
              success => #{type => boolean},
              data => #{type => object}
            }
          }
          }
        }
      }
    }
  }
  },
  PutData = #{put =>
  #{tags => ["Data"],
    description => <<"Post all the data">>,
    consumes => ["application/json"],
    produces => ["application/json"],
    requestBody =>  #{
      name => <<"Pages">>,
      in => body,
      description => <<" fcm data to add
        \nExample - {
      \n\"fcm_token\": \"fcm_token1\",
      \n\"luser\": \"luser\"
   }">>,

      required => false,
      content => #{
        'application/json' =>
        #{schema => #{
          type => object,
          properties => #{
            luser => #{type => string},
            fcm_token => #{type => string}
          }
        }
        }
      }
    },
    responses => #{
      <<"200">> => #{
        description => <<"">>,
        content => #{
          'application/json' =>
          #{schema => #{
            type => object,
            properties => #{
              success => #{type => boolean},
              total => #{type => integer},
              data => #{type => object}
            }
          }
          }
        }
      }
    }
  }
  },
  GetData = #{
    get =>
    #{
      tags => ["Data"],
      description => <<"Retrieve data">>,
      produces => ["application/json"],
      parameters => [
        #{name => <<"user_id">>, in => query, required => false, type => <<"string">>}
      ],
      responses => #{
        <<"200">> => #{
          description => <<"">>,
          content => #{
            'application/json' =>
            #{schema => #{
              type => object,
              properties => #{
                success => #{type => boolean},
                data => #{type => object}
              }
            }
            }
          }
        }
      }
    }
  },
  DeleteData = #{
    delete =>
    #{
      tags => ["Data"],
      description => <<"Delete the data">>,
      produces => ["application/json"],
      parameters => [
        #{name => <<"fcm_token">>, in => query, required => true, type => <<"string">>}
      ],
      responses => #{
        <<"200">> => #{
          description => <<"">>,
          content => #{
            'application/json' =>
            #{schema => #{
              type => object,
              properties => #{
                success => #{type => boolean},
                message => #{type => string}
              }
            }
            }
          }
        }
      }
    }
  },

  [
    trails:trail("/fcm/key", mod_fcm_handler, [],GetData),
    trails:trail("/fcm/delete", mod_fcm_handler, [],DeleteData),
    trails:trail("/fcm/add", mod_fcm_handler, [],PostData),
    trails:trail("/fcm/edit", mod_fcm_handler, [],PutData),
    trails:trail("/notify/send-message-push", mod_fcm_handler, [],PushData)
  ].

-spec handle({binary(), cowboy_req:req()}, term()) -> {ok, cowboy_req:req(), term()}.

handle({<<"POST">>, Req}, State) -> handle_post(Req, State);
handle({<<"GET">>, Req}, State) -> handle_get(Req, State);
handle({<<"DELETE">>, Req}, State) -> handle_delete(Req, State);
handle({<<"PUT">>, Req}, State) -> handle_put(Req, State);
handle({_, Req}, State) ->
  {ok, cowboy_req:reply(405, #{}, [], Req), State}.

handle_get(Req, State) ->
  {ok, which_get({cowboy_req:path(Req), Req}, State), State}.

-spec which_get({binary(), cowboy_req:req()}, term()) -> {ok, cowboy_req:req()}.
which_get({<<"/fcm/key">>, Req}, _State) ->
  Query = cowboy_req:parse_qs(Req),
  UserId = proplists:get_value(<<"luser">>, Query, <<"">>),
  case UserId of
    <<"">> ->
      Reply = #{success => false, error => <<"User id is not provided">>},
      cowboy_req:reply(400, reply_headers(), jsone:encode(Reply), Req);
    _ ->
      case mod_fcm_rdbms:selectFcmToken(UserId) of
        {ok, Data} ->
          Reply = #{success => true, data => Data},
          cowboy_req:reply(200, reply_headers(), jsone:encode(Reply),Req);
        error ->
          Reply = #{success => false, error => <<"Not found">>},
          cowboy_req:reply(404, reply_headers(), jsone:encode(Reply), Req)
      end
  end;

which_get({_, Req},  _State) ->
  cowboy_req:reply(404, #{}, [], Req).

handle_delete(Req, State) ->
  {ok, which_delete({cowboy_req:path(Req), Req}, State), State}.
%%--------------------------------------------------------------------
which_delete({<<"/fcm/delete">>, Req}, _State) ->
  Query = cowboy_req:parse_qs(Req),
  FcmToken = proplists:get_value(<<"fcm_token">>, Query, <<"">>),
  case FcmToken of
    <<"">> ->
      Reply = #{success => false, error => <<"Fcm token is not provided">>},
      cowboy_req:reply(400, reply_headers(), jsone:encode(Reply), Req);
    _ ->
      case mod_fcm_rdbms:deleteFcmToken(FcmToken) of
        ok ->
          Reply = #{success => true},
          cowboy_req:reply(200, reply_headers(), jsone:encode(Reply), Req);
        error ->
          Reply = #{success => false, error => <<"Not found">>},
          cowboy_req:reply(404, reply_headers(), jsone:encode(Reply), Req)
      end
  end;

which_delete({_, Req}, _State) ->
  cowboy_req:reply(404, #{}, [], Req).
handle_post(Req, State) ->
  {ok, Json, ReqF} = cowboy_req:read_body(Req),
  {ok, Params, _} = jsone_decode:decode(Json, [{object_format, proplist}]),
  {ok, which_post({cowboy_req:path(ReqF), ReqF}, Params, State), State}.

-spec which_post({binary(), cowboy_req:req()}, list(), term()) -> {ok, cowboy_req:req()}.
%%--------------------------------------------------------------------
which_post({<<"/fcm/add">>, Req}, Params, _State) ->
  FcmToken = proplists:get_value(<<"fcm_token">>, Params, <<"">>),
  LUser = proplists:get_value(<<"luser">>, Params, <<"">>),
  Resource = proplists:get_value(<<"resource">>, Params, <<"">>),
  Token = FcmToken,
  Device = proplists:get_value(<<"device_type">>, Params, <<"">>),
  DeviceType = string:lowercase(Device),
  Id = uuid:get_v4(),
  UserId = uuid:uuid_to_string(Id),
  AllowedDeviceTypes = [<<"android">>, <<"ios">>, <<"chrome">>],
  case {DeviceType, FcmToken, LUser, Resource} of
    {<<"">>, _, _, _} ->
      Reply = #{success => false, error => <<"Device type is not provided">>},
      cowboy_req:reply(400, reply_headers(), jsone:encode(Reply), Req);
    {_, <<"">>, _, _} ->
      Reply = #{success => false, error => <<"Fcm token is not provided">>},
      cowboy_req:reply(400, reply_headers(), jsone:encode(Reply), Req);
    {_, _, <<"">>, _} ->
      Reply = #{success => false, error => <<"Required LUser">>},
      cowboy_req:reply(400, reply_headers(), jsone:encode(Reply), Req);
    {_, _, _, <<"">>} ->
      Reply = #{success => false, error => <<"Required Resource">>},
      cowboy_req:reply(400, reply_headers(), jsone:encode(Reply), Req);
    _ ->
      case lists:member(DeviceType, AllowedDeviceTypes) of
        true ->
          case mod_fcm_rdbms:addFcmToken(UserId, FcmToken, DeviceType, Token, Resource, LUser) of
            ok ->
              Reply = #{success => true, data => {UserId,DeviceType,FcmToken,Token}},
              cowboy_req:reply(200, reply_headers(), jsone:encode(Reply), Req);
            error ->
              Reply = #{success => false, error => <<"Failed to add data">>},
              cowboy_req:reply(500, reply_headers(), jsone:encode(Reply), Req)
          end;
        false ->
          Reply = #{success => false, error => <<"Invalid device type">>},
          cowboy_req:reply(400, reply_headers(), jsone:encode(Reply), Req)
      end
  end;

which_post({<<"/notify/send-message-push">>, Req},Params, _State) ->
  LUser = proplists:get_value(<<"receiverId">>, Params, <<"">>),
  Message = proplists:get_value(<<"payload">>, Params, <<"">>),
  case {LUser, Message} of
    {<<"">>, _} ->
      Reply = #{success => false, error => <<"User id is not provided">>},
      cowboy_req:reply(400, reply_headers(), jsone:encode(Reply), Req);
    {_, <<"">>} ->
      Reply = #{success => false, error => <<"payload is not provided">>},
      cowboy_req:reply(400, reply_headers(), jsone:encode(Reply), Req);
    _ ->
      case mod_fcm_rdbms:selectToken(LUser) of
        {ok, [{Token, DeviceType}] } ->
          case mod_fcm_push_handler:send_push(Message, Token, DeviceType) of
            {error, Reason} ->
              Reply = #{success => false, error => Reason},
              cowboy_req:reply(400, reply_headers(), jsone:encode(Reply), Req);
            {ok, Response}->
              Reply = #{success => true, data => Response},
              cowboy_req:reply(200, reply_headers(), jsone:encode(Reply),Req)
          end;
        _ ->
          Reply = #{success => false, error => <<"Not found">>},
          cowboy_req:reply(404, reply_headers(), jsone:encode(Reply), Req)
      end
  end;

which_post({_, Req}, _Params, _State) ->
  cowboy_req:reply(404, #{}, [], Req).

-spec handle_put(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
handle_put(Req, State) ->
  {ok, Json, ReqF} = cowboy_req:read_body(Req),
  {ok, Params, _} = jsone_decode:decode(Json, [{object_format, proplist}]),
  {ok, which_put({cowboy_req:path(ReqF), ReqF}, Params, State), State}.

-spec which_put({binary(), cowboy_req:req()}, list(), term()) -> {ok, cowboy_req:req()}.
%%--------------------------------------------------------------------
which_put({<<"/fcm/edit">>, Req}, Params, _State) ->
  LUser = proplists:get_value(<<"luser">>, Params, <<"">>),
  case mod_fcm_rdbms:selectFcmToken(LUser) of
    {ok, []} ->
      Reply = #{success => false, error => <<"Not Found">>},
      cowboy_req:reply(400, reply_headers(), jsone:encode(Reply), Req);
    _ ->
      FcmToken = proplists:get_value(<<"fcm_token">>, Params, <<"">>),
      Token = FcmToken,
      case {LUser, FcmToken} of
        {<<"">>, _} ->
          Reply = #{success => false, error => <<"User id is not provided">>},
          cowboy_req:reply(400, reply_headers(), jsone:encode(Reply), Req);
        {_, <<"">>} ->
          Reply = #{success => false, error => <<"Fcm token is not provided">>},
          cowboy_req:reply(400, reply_headers(), jsone:encode(Reply), Req);
        _ ->
          case mod_fcm_rdbms:updateFcmToken(FcmToken, Token, LUser) of
            ok ->
              Reply = #{success => true, data => {LUser, FcmToken, Token}},
              cowboy_req:reply(200, reply_headers(), jsone:encode(Reply), Req);
            error ->
              Reply = #{success => false, error => <<"Failed to add data">>},
              cowboy_req:reply(400, reply_headers(), jsone:encode(Reply), Req)
          end
      end
  end.
%%====================================================================
-spec reply_headers() -> map().
%%--------------------------------------------------------------------
reply_headers() ->
  #{?CTYPE_KEY => ?JSON_MIME}.