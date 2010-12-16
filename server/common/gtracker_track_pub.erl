-module(gtracker_track_pub).

-include("common_recs.hrl").

-export([open/1, close/1, store/2, clear/1, get_coords/1, set_owner/2]).

open(Track) ->
   rpc:call(Track#track.node, gtracker_track, open, [Track#track.id, Track#track.path, self()]).

close(TrackPid) when is_pid(TrackPid) == false ->
   ok;
close(TrackPid) ->
   TrackPid ! {close, self()},
   ok.

store(TrackPid, _) when is_pid(TrackPid) == false ->
   ok;
store(TrackPid, Coord) ->
   TrackPid ! Coord,
   ok.

clear(TrackPid) when is_pid(TrackPid) == false ->
   ok;
clear(TrackPid) ->
   TrackPid ! clear.

get_coords(TrackPid) when is_pid(TrackPid) == false ->
   ok;
get_coords(TrackPid) ->
   TrackPid ! {get_coords, self()},
   receive
      {reply, Coords} ->
         Coords;
      Res ->
         {unexpected, Res}
   end.

set_owner(TrackPid, OwnerPid) ->
   TrackPid ! {owner, OwnerPid}.

%=======================================================================================================================
%  unit testing facilities
%=======================================================================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

track_test() ->
 Track = #track{id = 'track_1', path = "/tmp/track_1"},
 TrackPid = gtracker_track_pub:open(Track),
 ?assertEqual(TrackPid, gtracker_track_pub:open(Track)),
 gtracker_track_pub:close(TrackPid).

track1_test() ->
  Track = #track{id = 'track_2', path = "/tmp/track_2"},
  TrackPid = gtracker_track_pub:open(Track),
  gtracker_track_pub:clear(TrackPid),

  Coord1 = #coord{lat = 123.1, lon = 321.12, speed = 45, timestamp = now()},
  TrackPid ! Coord1,

  Coord2 = #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()},
  TrackPid ! Coord2,

  Coord3 = #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()},
  TrackPid ! Coord3,

  Coord4 = #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()},
  TrackPid ! Coord4,

  ?assertEqual(lists:sort([Coord1, Coord2, Coord3, Coord4]), lists:sort(gtracker_track_pub:get_coords(TrackPid))),

  gtracker_track_pub:clear(TrackPid),

  ?assertEqual([], lists:sort(get_coords(TrackPid))),

  gtracker_track_pub:close(TrackPid).

track2_test() ->
  Track = #track{id = 'track_2', path = "/tmp/track_3"},
  TrackPid = gtracker_track_pub:open(Track),
  gtracker_track_pub:clear(TrackPid),

  TrackPid ! #coord{lat = 123.1, lon = 321.12, speed = 45, timestamp = now()},
  TrackPid ! #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()},
  TrackPid ! #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()},
  TrackPid ! #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()},

  gtracker_track_pub:close(TrackPid),

  timer:sleep(200),

  TrackPid1 = gtracker_track_pub:open(Track),
  TrackPid2 = gtracker_track_pub:open(Track),

  Res1 = lists:sort(gtracker_track_pub:get_coords(TrackPid1)),
  Res2 = lists:sort(gtracker_track_pub:get_coords(TrackPid2)),
  ?assertEqual(Res1, Res2),

  gtracker_track_pub:close(TrackPid1),
  gtracker_track_pub:close(TrackPid2).

-endif.
