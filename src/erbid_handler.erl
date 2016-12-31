%%%-------------------------------------------------------------------
%%% @author khanhhua
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Dec 2016 11:59
%%%-------------------------------------------------------------------
-module(erbid_handler).
-author("khanhhua").

-import(cowboy_req, [reply/4]).

%% API
-export([init/3, handle/2, terminate/3]).

-record(state, {bidders}).

init(_Type, Req, _Opts) ->
  {ok, Req, no_state}.

handle(Req, State) ->
  {ok, Req2} = cowboy_req:reply(
    200,
    [
      {<<"content-type">>, <<"text/plain">>}
    ],
    <<"Welcome to ERbid!">>,
    Req),
  {ok, Req2, State}.

terminate(_Reason, Req, State) ->
  ok.