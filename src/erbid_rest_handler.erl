%%%-------------------------------------------------------------------
%%% @author khanhhua
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Dec 2016 11:59
%%%-------------------------------------------------------------------
-module(erbid_rest_handler).
-author("khanhhua").

-import(cowboy_req, [reply/4, binding/2]).

%% Cowboy behavior
-export([init/3, handle/2, terminate/3]).

-record(state, {bidders}).

init(_Type, Req, _Opts) ->
  {ok, Req, no_state}.

handle(Req, State) ->
  {ResourceName, _} = cowboy_req:binding(resourceName, Req),

  {ok, Req2} = case ResourceName of
    <<"listings">> -> handle_listings(Req, State);
    _ -> cowboy_req:reply(400,
            [
              {<<"content-type">>, <<"text/plain">>}
            ],
            <<"Unknown resource requested">>,
            Req)
  end,
  {ok, Req2, State}.

terminate(_Reason, Req, State) ->
  ok.

%% PRIVATE

%%-------------------------------------------------------------------
%%
%%-------------------------------------------------------------------
handle_listings(Req, State) ->
  cowboy_req:reply(200,
    [
      {<<"content-type">>, <<"application/json">>}
    ],
    <<"{\"listings\":[]}">>,
    Req).
