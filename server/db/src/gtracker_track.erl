-module(gtracker_track).

-include("common_recs.hrl").

-export([start/3, start/4, init/3, loop/1, clear/1, close/1, get_coords/1]).

start(Name, Node, Owner, Path) ->
   rpc:call(Node, gtracker_track, start, [Name, Owner, Path]).

start(Name, Owner, Path) ->
   spawn(fun() -> init(Name, Owner, Path) end).

init(Name, Owner, Path) ->
   link(Owner),
   {ok, Ref} = dets:open_file(Name, [{auto_save, 1000}, {file, Path}, {keypos, 5}]),
   loop(Ref).

loop(Name) ->
   receive
      Coord when is_record(Coord, coord) ->
         ok = dets:insert(Name, Coord),
         loop(Name);
      {get_coords, Peer} ->
         Coords = dets:select(Name, [{'_', [], ['$_']}]),
         Peer ! {reply, Coords},
         loop(Name);
      clear ->
         dets:delete_all_objects(Name),
         loop(Name);
      {'EXIT', _, _} ->
         dets:close(Name);
      exit ->
         dets:close(Name)
   end.

clear(TrackRef) ->
   TrackRef ! clear.

close(TrackRef) ->
   TrackRef ! exit.

get_coords(TrackRef) ->
   TrackRef ! {get_coords, self()},
   receive
      {reply, Coords} ->
         Coords;
      Res ->
         {unexpected, Res}
   end.

%=======================================================================================================================
%  unit testing facilities
%=======================================================================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

track_test() ->
   TrackPid = gtracker_track:start(tmp, self(), "/tmp/tmp.dets"),
   gtracker_track:clear(TrackPid),

   Coord1 = #coord{lat = 123.1, lon = 321.12, speed = 45, timestamp = now()},
   TrackPid ! Coord1,

   Coord2 = #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()},
   TrackPid ! Coord2,

   Coord3 = #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()},
   TrackPid ! Coord3,

   Coord4 = #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()},
   TrackPid ! Coord4,

   ?assertEqual(lists:sort([Coord1, Coord2, Coord3, Coord4]), lists:sort(get_coords(TrackPid))),

   gtracker_track:clear(TrackPid),

   ?assertEqual([], lists:sort(get_coords(TrackPid))),

   gtracker_track:close(TrackPid).

track2_test() ->
   TrackPid = gtracker_track:start(tmp, self(), "/tmp/tmp1.dets"),
   gtracker_track:clear(TrackPid),

   TrackPid ! #coord{lat = 123.1, lon = 321.12, speed = 45, timestamp = now()},
   TrackPid ! #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()},
   TrackPid ! #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()},
   TrackPid ! #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()},

   gtracker_track:close(TrackPid),

   timer:sleep(200),

   TrackPid1 = gtracker_track:start(tmp, self(), "/tmp/tmp1.dets"),
   TrackPid2 = gtracker_track:start(tmp, self(), "/tmp/tmp1.dets"),

   Res1 = lists:sort(gtracker_track:get_coords(TrackPid1)),
   Res2 = lists:sort(gtracker_track:get_coords(TrackPid2)),
   ?assertEqual(Res1, Res2),

   gtracker_track:close(TrackPid1),
   gtracker_track:close(TrackPid2).

-endif.
