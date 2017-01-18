%%%-------------------------------------------------------------------
%%% @author khanhhua
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Jan 2017 20:54
%%%-------------------------------------------------------------------
-author("khanhhua").

-record(userRegForm, {username, password}).
-record(user, {username, hashedpass}).
-record(listing, {id, owner_name, title, description, price, deadline, image_url}).
-record(bid, {id, listing_id, bidder_name, bid_value, created_at}).
-record(auction, {listing_id, pid}).

% TODO Add RecordType
listing_to_map(Record) ->
  erlang:display(Record),

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
