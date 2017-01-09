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

-import(crypto, [hash/2]).
-import(maps, [find/2]).
-import(cowboy_req, [reply/4, binding/2]).
-import(jsx, [is_json/1, encode/1, decode/2]).

%% Cowboy behavior
-export([init/3, handle/2, terminate/3]).

-record(state, {bidders}).
-record(userRegForm, {username, password}). %% TODO Define more user registration fields

init(_Type, Req, Options) ->
  DbRef = proplists:get_value(usersTable, Options),
  {ok, Req, #{dbRef => DbRef}}.

handle(Req, State) ->
  {ActionName, _} = cowboy_req:binding(actionName, Req),

  {ok, Req2} = case ActionName of
    <<"login">> -> handle_login(Req, State);
    <<"logout">> -> handle_logout(Req, State);
    <<"signup">> -> handle_signup(Req, State);
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

          {ok, DbRef} = maps:find(dbRef, State),
          case dets:match_object(DbRef, {user, Username, HashedPassword}) of
            {error, Reason} ->
              json(400, #{ok => false, reason => list_to_binary(Reason)}, Req);
            [] ->
              json(404, #{ok => false, reason => <<"Not found">>}, Req);
            [User] ->
              Authtoken = base64:encode(hash(Username)),
              json(200, #{ok => true, authtoken => Authtoken, username => Username}, Req)
          end;
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

json(Status, Body, Req) ->
  cowboy_req:reply(Status,
    [
      {<<"content-type">>, <<"application/json">>}
    ],
    jsx:encode(Body),
    Req).

hash(Username) ->
  crypto:hash(md5, Username).
