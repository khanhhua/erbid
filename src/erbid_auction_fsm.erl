%%%-------------------------------------------------------------------
%%% @author khanhhua
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jan 2017 20:57
%%%-------------------------------------------------------------------
-module(erbid_auction_fsm).
-author("khanhhua").

-behavior(gen_fsm).
-export([start/1, stop/1, bid/3, info/1]).
%% gen_fsm API
-export([init/1, handle_event/3, handle_event_sync/4, handle_info/3, terminate/3]).

%% States
-export([pristine/2, bidding/2, sold/2, closed/2, finalized/2]).

-include("erbid.hrl").
%% State data structure: A map
%% - listing => Listing
%% - bids => [Bid]
%% - winner => {Username, Price}
%% Bid :: {Username, BidValue, CreatedAt}

%% -spec start(#listing) -> Pid::pid().
start(Listing) ->
  Deadline = binary:bin_to_list(Listing#listing.deadline),
%%  Turns ISO-8601 Datetime string to Erlang Datetime tuple
  [Y,M,D,Hh,Mm,Ss] = lists:map(fun (X) -> element(1, string:to_integer(X)) end, string:tokens(Deadline, "-T:Z")),
  Listing2 = Listing#listing{deadline = {{Y,M,D},{Hh,Mm,Ss}}},

  {ok, Pid} = gen_fsm:start(?MODULE, Listing2, []),
  Pid.

%% -spec bid(Pid, Username, BidValue) -> ok | {error, Reason}
bid(Pid, Username, BidValue) ->
  gen_fsm:send_event(Pid, {bid, Username, BidValue}).

%% -spec info(Pid) -> State | {error, Reason}.
info(Pid) ->
  {CurrentStateName, CurrentStateData} = sys:get_state(Pid),
  {ok, CurrentStateName, CurrentStateData}.

%% -spec stop(Pid) -> ok | {error, Reason}.
stop(Pid) ->
  gen_fsm:stop(Pid).

%%%     ==> {ok, StateName, State}
%%%         {ok, StateName, State, Timeout}
%%%         ignore
%%%         {stop, Reason}
init(Listing) ->
  State = #{
    listing => Listing,
    bids => [],
    winner => {undefined, undefined}
  },
  {ok, pristine, State, 60000}.


%%% StateName function
%%%    ==> {next_state, NewStateName, NewState}
%%%        {next_state, NewStateName, NewState, Timeout}
%%%        {stop, Reason, NewState}
%%%              Reason = normal | shutdown | Term terminate(State) is called

%%%------------
%%% @doc
%%% Auction has started and no bid has been made.
%%% Once the first bid is made, fsm moves onto "bidding"
%%% @end
%%%-------------
pristine({bid, Username, BidValue}, State) ->
  erlang:display("[pristine] Receiving..."),
  erlang:display({bid, Username, BidValue}),
  NewState = State#{bids := [{Username, BidValue}], winner := {Username, BidValue}},
  {next_state, bidding, NewState, 5000};
pristine(timeout, State) ->
  erlang:display("[pristine] Receiving timeout..."),
  #{listing := Listing} = State,
  Deadline = Listing#listing.deadline,

%% Cap between the two Bounds
  TimeoutLower = 5,
  TimeoutUpper = 3600,
  case calendar:time_difference(calendar:universal_time(), Deadline) of
    {0, {0, 0, Difference}} when Difference > 5 ->
      {next_state, pristine, State, max(TimeoutLower, min(Difference, TimeoutUpper))}
    ;
    {Difference, {_, _, _}} when Difference < 0 -> {next_state, closed, State, 5000}
    ;
    _ -> {next_state, closed, State, 5000}
  end.

%%%-------------
%%% @doc
%%% Auction is on-going until deadline is met
%%% @end
%%%-------------
bidding({bid, Username, BidValue}, State) ->
  erlang:display("[bidding]"),
  #{bids := Bids} = State,
  NewBids = lists:reverse(lists:keysort(2, [{Username, BidValue} | Bids])),

  [WinningBid | _] = NewBids,
  NewState = State#{bids := NewBids, winner := WinningBid},

  {next_state, bidding, NewState, 5000};

bidding(timeout, State) ->
%%  TODO Compare Deadline and Now to determine next state
  erlang:display("[bidding] Receving timeout"),
  #{listing := Listing} = State,
  Deadline = Listing#listing.deadline,

  case calendar:time_difference(Deadline, calendar:universal_time()) of
    {0, {0, 0, Difference}} when Difference < 5 ->
      {next_state, sold, State, 60000}
    ;
    _ -> {next_state, bidding, State, 5000}
  end.

%%% Deadline has reached. Auction item is now sold
sold(Msg, State) ->
%%  TODO Email winner
%%  TODO Email participants
%%  TODO Email seller
  {next_state, finalized, State, 60000}.

closed(Msg, State) ->
  erlang:display("[closed] Transitioning to finalized in 60000ms"),
  {next_state, finalized, State, 60000}.

finalized(Msg, State) ->
  erlang:display("[finalized]"),
  case Msg of
    timeout -> {stop, normal, State};
    _ -> {stop, forced_stop, State}
  end.

%%%
%%%    ==> {next_state, NewStateName, NewState}
%%%        {next_state, NewStateName, NewState, Timeout}
%%%        {stop, Reason, Reply, NewState}
%%%        {stop, Reason, NewState}
%%%              Reason = normal | shutdown | Term terminate(State) is called
handle_event(Msg, StateName, State) ->
  erlang:display("[handle_event]"),
  case Msg of
    stop ->
      erlang:display("[handle_event] Stopping on demand"),
      {stop, normal, State}
    ;
    _ -> {next_state, StateName, State}
  end.


%%%
%%%    ==> {next_state, NewStateName, NewState}
%%%        {next_state, NewStateName, NewState, Timeout}
%%%        {reply, Reply, NewStateName, NewState}
%%%        {reply, Reply, NewStateName, NewState, Timeout}
%%%        {stop, Reason, Reply, NewState}
%%%        {stop, Reason, NewState}
%%%              Reason = normal | shutdown | Term terminate(State) is called
handle_event_sync(Msg, From, StateName, State) ->
  {next_state, StateName, State}.

%%%   (e.g. {'EXIT', P, R}, {nodedown, N}, ...
%%%    ==> {next_state, NewStateName, NewState}
%%%        {next_state, NewStateName, NewState, Timeout}
%%%        {stop, Reason, NewState}
%%%              Reason = normal | shutdown | Term terminate(State) is called
handle_info(Info, StateName, State) ->
  erlang:display("[handle_info] Showing info"),
  erlang:display(State),
  {next_state, StateName, State}.

%%%  Let the user module clean up
%%%        always called when server terminates
%%%
%%%    ==> the return value is ignored
terminate(Reason, StateName, State) ->
%%  TODO Clean up
  erlang:display("[terminate]"),
  ok.
