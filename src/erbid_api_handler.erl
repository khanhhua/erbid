%%%-------------------------------------------------------------------
%%% @author khanhhua
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Dec 2016 11:59
%%%-------------------------------------------------------------------
-module(erbid_api_handler).
-author("khanhhua").

-include("erbid.hrl").

-import(crypto, [hash/2]).
-import(maps, [find/2]).
-import(cowboy_req, [reply/4, binding/2]).
-import(jsx, [is_json/1, encode/1, decode/2]).

%% Cowboy behavior
-export([init/3, handle/2, terminate/3]).

-record(state, {bidders}).

init(_Type, Req, Options) ->
  Ctx = proplists:get_value(hashidsCtx, Options),

  DbRef = proplists:get_value(usersTable, Options),
  ListingsTableRef = proplists:get_value(listingsTable, Options),
  BidsTableRef = proplists:get_value(bidsTable, Options),
  SessionsRef = proplists:get_value(sessions, Options),

  State = #{
    hashidsCtx => Ctx,
    sessions => SessionsRef,
    dbRef => DbRef,
    usersTable => DbRef,
    listingsTable => ListingsTableRef,
    bidsTable => BidsTableRef
  },
  {ok, Req, State}.

handle(Req, State) ->
  {ActionName, _} = cowboy_req:binding(actionName, Req),

  {ok, Req2} = case ActionName of
    <<"login">> -> handle_login(Req, State);
    <<"logout">> -> handle_logout(Req, State);
    <<"signup">> -> handle_signup(Req, State);
    <<"placeBid">> -> place_bid(Req, State);
    _ -> cowboy_req:reply(400,
            [
              {<<"content-type">>, <<"text/plain">>}
            ],
            <<"Invalid API call">>,
            Req)
  end,
  {ok, Req2, State}.

terminate(_Reason, Req, State) ->
  ok.

%% PRIVATE HANDLERS
handle_signup(Req, State) ->
  {ok, Data, _} = cowboy_req:body(Req),
  case jsx:is_json(Data) of
    false -> json(400, <<"Invalid JSON">>, Req);
    true ->
      UserRegForm = jsx:decode(Data, [return_maps]),
      ValidatedUserForm = validate_userform(UserRegForm),
      {ok, DbRef} = maps:find(dbRef, State),
      if
        ValidatedUserForm =:= false -> json(400, <<"Invalid user registration form">>, Req);
        true -> try register_new_user(UserRegForm, DbRef) of
                  {ok, User} -> json(200, User, Req);
                  {error, exists} -> json(400, <<"Duplicate user">>, Req);
                  {error, Reason} -> json(400, Reason, Req)
                catch
                  %%throw:slice -> "It is but a scratch."; as in throw(slice);
                  %%error:cut_arm -> "I've had worse."; as in erlang:error(cut_arm);
                  %%exit:cut_leg -> "Come on you pansy!"; as in exit(cut_leg);
                  %%_:_ -> "Just a flesh wound."
                  error:Error -> json(400, Error, Req)
                end
      end
  end.

validate_userform(UserRegForm) ->
  #{<<"username">> := Username, <<"password">> := Password} = UserRegForm,

  if
    Username =:= <<"">> -> false;
    Password =:= <<"">> -> false;
    true -> true
  end.

register_new_user(UserRegForm, DbRef) ->
  #{<<"username">> := Username, <<"password">> := Password} = UserRegForm,
  erlang:display(UserRegForm),

  HashedPassword = hash(Password),
  case dets:insert(DbRef, {user, Username, HashedPassword}) of
    ok -> {ok, #{username => Username}};
    {error, Reason} -> {error, Reason}
  end.

handle_login(Req, State) ->
  erlang:display("[handle_login] Starting"),
  {ok, Data, _} = cowboy_req:body(Req),
  case jsx:is_json(Data) of
    false -> json(400, <<"Invalid JSON">>, Req);
    true -> Credential = jsx:decode(Data, [return_maps]),
      case maps:find(<<"username">>, Credential) of
        {ok, Username} ->
          Password = case maps:find(<<"password">>, Credential) of
                       error -> <<"">>;
                       {ok, Value} -> Value
                     end,
          HashedPassword = hash(Password),

          #{dbRef     := DbRef,
            sessions  := SessionsTable} = State,
          case dets:match_object(DbRef, {user, Username, HashedPassword}) of
            {error, Reason} ->
              json(400, #{ok => false, reason => list_to_binary(Reason)}, Req);
            [] ->
              json(404, #{ok => false, reason => <<"Not found">>}, Req);
            [User] ->
              Authtoken = base64:encode(encrypt(Username)),
              erlang:display("[handle_login] Token " ++ binary_to_list(Authtoken)),
              case ets:insert(SessionsTable, {Authtoken, Username}) of
                true ->
                  erlang:display("[handle_login] SessionsTable updated"),
                  json(200, #{ok => true, authtoken => Authtoken, username => Username}, Req)
                ;
                false -> json(400, #{ok => false}, Req)
              end
          end
        ;
        _ ->
          json(400, <<"Missing username in payload">>, Req)
      end
  end.

handle_logout(Req, State) ->
  cowboy_req:reply(200,
    [
      {<<"content-type">>, <<"application/json">>}
    ],
    <<"logout() OK">>,
    Req).

place_bid(Req, State) ->
  %% TODO Upgrade database to Mnesia
  #{usersTable := UsersTableRef,
    listingsTable := ListingsTableRef,
    bidsTable := BidsTableRef} = State,

  {user, Username} = erbid_http:get_user(Req, State),

  {ok, Data, _} = cowboy_req:body(Req),
  %% TODO Is JSON?

  #{<<"listing_id">> := ListingId,
    <<"bid_value">>  := BidValue} = jsx:decode(Data, [return_maps]),

  {A, B, C} = now(),
  #{hashidsCtx := Ctx} = State,
  Id = list_to_binary(hashids:encode(Ctx, A * B * C)),

  CreatedAt = timestamp(),
  Bid = #bid{id = Id,
             listing_id = ListingId,
             bidder_name = Username,
             bid_value = BidValue,
             created_at = CreatedAt},
  case dets:insert_new(BidsTableRef, Bid) of
    true -> json(200, <<"ok">>, Req)
    ;
    false -> json(400, <<"error">>, Req)
  end.

json(Status, Body, Req) ->
  cowboy_req:reply(Status,
    [
      {<<"content-type">>, <<"application/json">>}
    ],
    jsx:encode(Body),
    Req).

hash(Username) ->
  crypto:hash(md5, Username).

encrypt(Username) when is_binary(Username) ->
%%  {ok, PkeyBin} = file:read_file('priv/erbid_privkey'),
%%  [PrivateKey] = public_key:pem_decode(PkeyBin),
%%  Token = crypto:private_encrypt(rsa, Username, PrivateKey, rsa_no_padding),
  Token = Username,
  Token.

decrypt(Token) ->
  Username = Token,
  Username.

timestamp() ->
  {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(erlang:now()),
  lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second])).
