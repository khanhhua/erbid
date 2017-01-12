%%%-------------------------------------------------------------------
%%% @author khanhhua
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Jan 2017 00:26
%%%-------------------------------------------------------------------
-module(erbid_http).
-author("khanhhua").

%% API
-export([get_user/2]).

get_user(Req, State) ->
  %% TODO Refactor to middleware
  {Authorization, _} = cowboy_req:header(<<"authorization">>, Req),
  <<"bearer ", Bearer/binary>> = Authorization,

  #{sessions := SessionsTable} = State,

  %%  User = dets:match_object(UsersTable, {user, Username, '$2'}),
  case ets:lookup(SessionsTable, Bearer) of
    [] -> {error, invalid_session};
    [Entry] ->
      {_, Username} = Entry,
      {user, Username}
  end.

