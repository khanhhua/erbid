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
-export([get_user/2, parse_int/1, parse_float/1]).

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

parse_int(Input) when is_binary(Input) ->
  try string:to_integer(binary_to_list(Input)) of
    {Integer, _} -> Integer
  catch
    _:_ -> 0
  end;
parse_int(Input) ->
  0.

parse_float(Input) when is_binary(Input) ->
  Bin = binary_to_list(Input),
  try string:to_float(Bin) of
    {error, no_float} -> case string:to_integer(Bin) of
                           {error, no_integer} -> 0;
                           {Out, _} -> Out
                         end
    ;
    {Out, _} -> Out
  catch
    _:_ -> 0
  end;
parse_float(Input) ->
  0.
