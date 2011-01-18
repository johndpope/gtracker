-module(gtracker_track_pub).

-include("common_recs.hrl").
-include("common_defs.hrl").

-export([close/1, store/2, clear/1, set_owner/2, set_subscribers/2, coords/1, stat/1]).

close(#track{id = TrackId, track_server = TrackServer}) ->
   gen_server:call(TrackServer, {close, TrackId}).

store(#track{id = TrackId, track_server = TrackServer}, Coord) ->
   gen_server:cast(TrackServer, Coord#coord{track_id = TrackId}).

clear(#track{id = TrackId, track_server = TrackServer}) ->
   gen_server:call(TrackServer, {clear, TrackId}).

set_owner(#track{id = TrackId, track_server = TrackServer}, OwnerPid) ->
   gen_server:call(TrackServer, {owner, OwnerPid, TrackId}).

set_subscribers(#track{id = TrackId, track_server = TrackServer}, Subscribers) ->
   gen_server:call(TrackServer, {subscribers, TrackId, Subscribers}).

stat(#track{id = TrackId, track_server = TrackServer}) ->
   gen_server:call(TrackServer, {get_track_stat, TrackId}).

coords(#track{id = TrackId, track_server = TrackServer}) ->
   gen_server:call(TrackServer, {get_track, TrackId}).
