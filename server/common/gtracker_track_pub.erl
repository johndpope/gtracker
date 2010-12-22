-module(gtracker_track_pub).

-include("common_recs.hrl").

-export([open/2, close/1, store/2, clear/1, get_coords/1, set_owner/2]).

open(Db, Track) ->
   Pid = rpc:call(Track#track.node, gtracker_track, open, [Db, Track#track.id, Track#track.path, self()]),
   Track#track{pid = Pid}.

close(#track{pid = Pid}) when is_pid(Pid) == false ->
   wrong_pid;
close(#track{pid = Pid}) ->
   Pid ! {close, self()},
   ok.

store(#track{pid = Pid}, _) when is_pid(Pid) == false ->
   wrong_pid;
store(#track{pid = Pid}, Coord) ->
   Pid ! Coord,
   ok.

clear(#track{pid = Pid}) when is_pid(Pid) == false ->
   wrong_pid;
clear(#track{pid = Pid}) ->
   Pid ! clear,
   ok.

get_coords(#track{pid = Pid}) when is_pid(Pid) == false ->
   wrong_pid;
get_coords(#track{pid = Pid}) ->
   Pid ! {get_coords, self()},
   receive
      {reply, Coords} ->
         Coords;
      Res ->
         {unexpected, Res}
   end.

set_owner(#track{pid = Pid}, _OwnerPid) when is_pid(Pid) == false ->
   wrong_pid;
set_owner(#track{pid = Pid}, OwnerPid) ->
   Pid ! {owner, OwnerPid}.

%=======================================================================================================================
%  unit testing facilities
%=======================================================================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("common_defs.hrl").

track_test() ->
 TmpTrack = #track{id = 'track_1', path = "/tmp/track_1"},
 Track = gtracker_track_pub:open(?db_ref, TmpTrack),
 ?assertEqual(Track, gtracker_track_pub:open(?db_ref, TmpTrack)),
 gtracker_track_pub:close(Track).

track1_test() ->
  TmpTrack = #track{id = 'track_2', path = "/tmp/track_2"},
  Track = gtracker_track_pub:open(?db_ref, TmpTrack),
  gtracker_track_pub:clear(Track),

 Coord1 = #coord{lat = 123.1, lon = 321.12, speed = 45, timestamp = now()},
 gtracker_track_pub:store(Track, Coord1),

 Coord2 = #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()},
 gtracker_track_pub:store(Track, Coord2),

 Coord3 = #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()},
 gtracker_track_pub:store(Track, Coord3),

 Coord4 = #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()},
 gtracker_track_pub:store(Track, Coord4),

 ?assertEqual(lists:sort([Coord1, Coord2, Coord3, Coord4]), lists:sort(gtracker_track_pub:get_coords(Track))),

 gtracker_track_pub:clear(Track),

 ?assertEqual([], lists:sort(get_coords(Track))),

 gtracker_track_pub:close(Track).

track2_test() ->
 TmpTrack = #track{id = 'track_2', path = "/tmp/track_3"},
 Track = gtracker_track_pub:open(?db_ref, TmpTrack),
 gtracker_track_pub:clear(Track),

 gtracker_track_pub:store(Track, #coord{lat = 123.1, lon = 321.12, speed = 45, timestamp = now()}),
 gtracker_track_pub:store(Track, #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()}),
 gtracker_track_pub:store(Track, #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()}),
 gtracker_track_pub:store(Track, #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()}),

 gtracker_track_pub:close(Track),

 timer:sleep(200),

 Track1 = gtracker_track_pub:open(?db_ref, Track),
 Track2 = gtracker_track_pub:open(?db_ref, Track),

 Res1 = lists:sort(gtracker_track_pub:get_coords(Track1)),
 Res2 = lists:sort(gtracker_track_pub:get_coords(Track2)),
 ?assertEqual(Res1, Res2),

 gtracker_track_pub:close(Track1),
 gtracker_track_pub:close(Track2).

-endif.
