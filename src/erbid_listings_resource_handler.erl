%%%-------------------------------------------------------------------
%%% @author khanhhua
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Jan 2017 00:28
%%%-------------------------------------------------------------------
-module(erbid_listings_resource_handler).
-author("khanhhua").

-import(cowboy_req, [binding/2, set_resp_body/2]).
-import(jsx, [is_json/1, encode/1, decode/2]).

-record(listing, {id, title, description, price, deadline, image_url}).

%% BEHAVIORS
-export([
  init/3,
  rest_init/2,
  terminate/3,
  content_types_provided/2,
  content_types_accepted/2,
  allowed_methods/2
]).

%% HANDLERS
-export([to_html/2, to_json/2, create_listing/2]).

%%-spec init({TransportName, ProtocolName}, Req, Options) ->
%%  {upgrade, protocol, cowboy_rest} | {upgrade, protocol, cowboy_rest, Req, Options}.
init({TransportName, ProtocolName}, Req, Options) ->
  DbRef = proplists:get_value(dbRef, Options),
  HashidsCtx = proplists:get_value(hashidsCtx, Options),

  State = #{dbRef => DbRef, hashidsCtx => HashidsCtx},
  erlang:display("init/3"),

  {upgrade, protocol, cowboy_rest, Req, State}.

rest_init(Req, State) ->
  erlang:display("rest_init"),
  {ok, Req, State}.

terminate(_Reason, Req, State) ->
  ok.

allowed_methods(Req, State) ->
  erlang:display("allowed_methods"),
  {[<<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
  erlang:display("content_types_provided"),

  {[
    {<<"text/html">>, to_html},
    {{<<"application">>, <<"json">>, '*'}, to_json}
  ], Req, State}.

content_types_accepted(Req, State) ->
  erlang:display("content_types_accepted"),

  {[
    {{<<"application">>, <<"json">>, '*'}, create_listing}
  ], Req, State}.

to_html(Req, State) -> to_json(Req, State).

to_json(Req, State) ->
  erlang:display("to_json"),

  {Id, _} = cowboy_req:binding(id, Req),
  erlang:display(Id),
  if
    Id =:= undefined ->
      Body = case query_for_listings(Req, State) of
        {ok, []} -> <<"{\"listings\": []}">>;
        {ok, Listings} -> jsx:encode(#{<<"listings">> => lists:map(fun listing_to_map/1, Listings)})
      end,
      {Body, Req, State};
    true ->
      {ok, One} = get_listing(Id, State),
      Body = jsx:encode(#{<<"listing">> => One}),
      {Body, Req, State}
  end.

create_listing(Req, State) ->
  erlang:display("create_listing"),
  {ok, Body, _} = cowboy_req:body(Req),

  case jsx:is_json(Body) of
    false -> {false, Req, State};
    true ->
      Data = jsx:decode(Body, [return_maps]),
      #{<<"title">> := Title,
        <<"description">> := Description,
        <<"price">> := Price,
        <<"deadline">> := Deadline,
        <<"image_url">> := ImageUrl} = Data,
      {ok, DbRef} = maps:find(dbRef, State),
      #{hashidsCtx := Ctx} = State,
      Id = list_to_binary(hashids:encode(Ctx, round(rand:uniform() * 1000000))),

      Listing = #listing{
        id=Id,
        title=Title,
        description=Description,
        price=Price,
        deadline=Deadline,
        image_url=ImageUrl
      },
      case dets:insert_new(DbRef, Listing) of
        true ->
          Json = listing_to_map(Listing),
          Req2 = cowboy_req:set_resp_body(jsx:encode(Json), Req),
          {true, Req2, State};
        false -> {false, Req, State};
        {error, Reason} -> {false, Req, State}
      end
  end.

query_for_listings(Req, State) ->
  {Query, _} = cowboy_req:qs_val(<<"q">>, Req),

  {ok, DbRef} = maps:find(dbRef, State),

%%  Listings = case dets(DbRef, [{#listing{
%%                id='_',
%%                title='_',
%%                description='_',
%%                price='_',
%%                deadline='_',
%%                image_url='_'}, [], ['$_']}
%%            ], 10) of
%%              '$end_of_table' -> [];
%%              {[Listing], Continuation} -> page([Listing], Continuation)
%%             end,
%%  Listings = dets:foldl(fun (Listing, AccIn) -> Listing ++ AccIn end, [], DbRef),
  Listings = dets:foldr(fun(X, L) -> [X|L] end, [], DbRef),
  erlang:display(Listings),
  {ok, Listings}.

page (Items, Continuation2) ->
  erlang:display(Items),
  erlang:display(Continuation2),
  case dets:select(Continuation2) of
    {Listing2, Continuation3} ->
      erlang:display(Listing2),
      page(Listing2 ++ Items, Continuation3);
    '$end_of_table' -> Items
  end.

get_listing(Id, State) ->
  {ok, DbRef} = maps:find(dbRef, State),

  case dets:match_object(DbRef, {listing, id, Id}) of
    [Listing] -> Listing;
    [] -> {error, notfound}
  end.

% TODO Add RecordType
listing_to_map(Record) ->
  Fields = record_info(fields, listing),
  Size = record_info(size, listing) - 1,
  Indices = lists:seq(1, Size),

  Map2 = lists:foldl(
  fun (Index, Map) ->
  Key = lists:nth(Index, Fields),
  Value = element(Index + 1, Record),
  maps:put(Key, Value, Map)
  end, #{}, Indices),

  Map2.
