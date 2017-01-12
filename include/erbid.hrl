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
