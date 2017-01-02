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

init(_Type, Req, _Opts) ->
  {ok, Req, no_state}.

handle(Req, State) ->
  {ActionName, _} = cowboy_req:binding(actionName, Req),

  {ok, Req2} = case ActionName of
    <<"login">> -> handle_login(Req, State);
    <<"logout">> -> handle_logout(Req, State);
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

%% PRIVATE
handle_login(Req, State) ->
  {ok, Data, _} = cowboy_req:body(Req),
  case jsx:is_json(Data) of
    false -> cowboy_req:reply(400,
      [
        {<<"content-type">>, <<"application/json">>}
      ],
      <<"Invalid JSON">>,
      Req);
    true -> Credential = jsx:decode(Data, [return_maps]),
      case maps:find(<<"username">>, Credential) of
        {ok, Username} ->
          Authtoken = base64:encode(hash(Username)),
          true = ets:insert(users, {binary_to_list(Authtoken), binary_to_list(Username)}),

          cowboy_req:reply(200,
            [
              {<<"content-type">>, <<"application/json">>}
            ],
            jsx:encode(#{ok => true, authtoken => Authtoken, username => Username}),
            Req);
        _ ->
          cowboy_req:reply(400,
            [
              {<<"content-type">>, <<"application/json">>}
            ],
            <<"Missing username in payload">>,
            Req)
      end
  end.

handle_logout(Req, State) ->
  cowboy_req:reply(200,
    [
      {<<"content-type">>, <<"application/json">>}
    ],
    <<"logout() OK">>,
    Req).


hash(Username) ->
  crypto:hash(md5, Username).
