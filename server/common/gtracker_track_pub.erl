-module(gtracker_track_pub).

-include("common_recs.hrl").
-include("common_defs.hrl").

-export([close/1, store/2, clear/1, set_owner/2, set_subscribers/2, coords/1, stat/1]).

close(#track{id = TrackId, node = Node}) ->
   gen_server:call({?track_ref, Node}, {close, TrackId}).

store(#track{id = TrackId, node = Node}, Coord) ->
   gen_server:call({?track_ref, Node}, Coord#coord{track_id = TrackId}).

clear(#track{id = TrackId, node = Node}) ->
   gen_server:call({?track_ref, Node}, {clear, TrackId}).

set_owner(#track{id = TrackId, node = Node}, OwnerPid) ->
   gen_server:call({?track_ref, Node}, {owner, OwnerPid, TrackId}).

set_subscribers(#track{id = TrackId, node = Node}, Subscribers) ->
   gen_server:call({?track_ref, Node}, {subscribers, TrackId, Subscribers}).

stat(#track{id = TrackId, node = Node}) ->
   gen_server:call({?track_ref, Node}, {get_track_stat, TrackId}).

coords(#track{id = TrackId, node = Node}) ->
   gen_server:call({?track_ref, Node}, {get_track, TrackId}).
