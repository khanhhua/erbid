%%%-------------------------------------------------------------------
%%% @author khanhhua
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jan 2017 17:39
%%%-------------------------------------------------------------------
-module(erbid_auction_sup).
-author("khanhhua").

-behaviour(supervisor).

%% API
-export([start_link/0, start_auction/1, auction_info/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-include("erbid.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  ets:new(?SERVER, [set, named_table]),
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_auction(Listing) ->
  ChildId = Listing#listing.id,

  case auction_info(ChildId) of
    {ok, StateName, State} ->
      {error, already_started}
    ;
    {error, _} ->
      StartFunc = {erbid_auction_fsm, start, [Listing]},
      Restart = permanent,
      Shutdown = 2000,
      Type = worker,
      Modules = [],

      ChildSpec = {ChildId, StartFunc, Restart, Shutdown, Type, Modules},
      erlang:display("Starting supervised auction with params"),
      erlang:display(ChildSpec),

      case supervisor:start_child(?SERVER, ChildSpec) of
        {ok, ChildPid} ->
          ets:insert(?SERVER, {ChildId, ChildPid}),
          {ok, ChildPid}
        ;
        {error, Error} -> {error, Error}
      end
  end.

-spec(auction_info(AuctionId :: list()) ->
  {ok, StateName :: term(), State :: term()} |
  {error, Reason :: term()}).
auction_info(AuctionId) ->
  case ets:lookup(?SERVER, AuctionId) of
    [AuctionProcess] ->
      {_, Pid} = AuctionProcess,
      erbid_auction_fsm:info(Pid)
    ;
    [] -> {error, not_exists}
    ;
    {error, Reason} -> {error, Reason}
  end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {
    SupFlags :: {
      RestartStrategy :: supervisor:strategy(),
      MaxR :: non_neg_integer(),
      MaxT :: non_neg_integer()
    },
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 100,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
  {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
