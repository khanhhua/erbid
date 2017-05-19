%%%-------------------------------------------------------------------
%%% @author khanhhua
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Dec 2016 10:56
%%%-------------------------------------------------------------------
-module(erbid).
-author("khanhhua").

-behaviour(application).

-import(cowboy_router, [compile/1]).
-import(cowboy, [start_http/4]).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
  ListingsTable = '../../../../data/listings.data',
  UsersTable    = '../../../../data/users.data',
  BidsTable     = '../../../../data/bids.data',

  {ok, ListingsTableRef} = dets:open_file(ListingsTable, [{type, set}, {keypos, 2}]),
  {ok, UsersTableRef} = dets:open_file(UsersTable, [{type, bag}, {keypos, 2}]),
  {ok, BidsTableRef} = dets:open_file(BidsTable, [{type, bag}, {keypos, 2}]),
  SessionsTable = ets:new(sessions, [set, named_table, public]),

  HashidsCtx = hashids:new([{salt, "SALTYASSALT"}]),
  Dispatch = cowboy_router:compile([
    %% {HostMatch, list({PathMatch, Handler, Opts})}
    {'_', [
      {"/", cowboy_static, {priv_file, erbid, "static/index.html"}},
      {"/app", cowboy_static, {priv_file, erbid, "static/app.html"}},
      {"/assets/[...]", cowboy_static, {priv_dir, erbid, "static/assets"}},
      {"/demo/[...]", cowboy_static, {priv_dir, erbid, "static/demo"}},
      {"/api/resources/listings/[:id]", erbid_listings_resource_handler,
        [
          {listingsTable, ListingsTableRef},
          {hashidsCtx, HashidsCtx},
          {sessions, SessionsTable}
        ]},
      %%{"/api/resources/:resourceName/[:id]", erbid_rest_handler, [{dbRef, DatabaseRef}]},
      {"/api/:actionName", erbid_api_handler,
        [
          {hashidsCtx, HashidsCtx},
          {sessions, SessionsTable},
          {usersTable, UsersTableRef},
          {listingsTable, ListingsTableRef},
          {bidsTable, BidsTableRef}
        ]}
    ]}
  ]),
  cowboy:start_http(my_http_listener, 100, [{port, 8080}],
    [
      {
        env, [
          {dispatch, Dispatch}
        ]
      }
    ]
  ),

  erbid_sup:start_link().



%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
