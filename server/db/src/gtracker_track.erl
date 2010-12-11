-module(gtracker_track).

-include("common_recs.hrl").

-include_lib("eunit/include/eunit.hrl").

-export([open/2, open/1, close/1, store/2, init/2, loop/1, clear/1, get_coords/1]).

open(Track) ->
   rpc:call(Track#track.node, gtracker_track, open, [Track#track.id, Track#track.path]).

close(TrackPid) ->
   TrackPid ! close.

open(TrackId, Path) ->
   PidName = mds_utils:list_to_atom(TrackId),
   case erlang:whereis(PidName) of
      undefined ->
         Pid = spawn_link(fun() -> init(TrackId, Path) end),
         register(PidName, Pid),
         Pid;
      Pid ->
         Pid
   end.

init(TrackId, Path) ->
   {ok, Ref} = dets:open_file(TrackId, [{auto_save, 1000}, {file, Path}, {keypos, 5}]),
   loop(Ref).

loop(Ref) ->
   receive
      %M = {get_coords, Peer} ->
      %   ?debugFmt("~p", [M]),
      %   Peer ! {reply, []},
      %   loop(Name);
      %Msg ->
      %   ?debugFmt("~p", [Msg]),
      %   loop(Name)
      Coord when is_record(Coord, coord) ->
        ok = dets:insert(Ref, Coord),
        loop(Ref);
      {get_coords, Peer} ->
        Coords = dets:select(Ref, [{'_', [], ['$_']}]),
        Peer ! {reply, Coords},
        loop(Ref);
      clear ->
        dets:delete_all_objects(Ref),
        loop(Ref);
      close ->
        dets:close(Ref);
      {'EXIT', _, _} ->
        dets:close(Ref)
   end.

store(TrackPid, Coord) ->
   TrackPid ! Coord.

clear(TrackPid) ->
   TrackPid ! clear.

get_coords(TrackPid) ->
   TrackPid ! {get_coords, self()},
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
 Track = #track{id = "track_1", path = "/tmp/track_1"},
 TrackPid = gtracker_track:open(Track),
 ?assertEqual(TrackPid, gtracker_track:open(Track)),
 gtracker_track:close(TrackPid).

track1_test() ->
   Track = #track{id = "track_2", path = "/tmp/track_2"},
   TrackPid = gtracker_track:open(Track),
   gtracker_track:clear(TrackPid),

   Coord1 = #coord{lat = 123.1, lon = 321.12, speed = 45, timestamp = now()},
   TrackPid ! Coord1,

   Coord2 = #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()},
   TrackPid ! Coord2,

   Coord3 = #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()},
   TrackPid ! Coord3,

   Coord4 = #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()},
   TrackPid ! Coord4,

   ?assertEqual(lists:sort([Coord1, Coord2, Coord3, Coord4]), lists:sort(gtracker_track:get_coords(TrackPid))),

   gtracker_track:clear(TrackPid),

   ?assertEqual([], lists:sort(get_coords(TrackPid))),

   gtracker_track:close(TrackPid).

track2_test() ->
   Track = #track{id = "track_2", path = "/tmp/track_3"},
   TrackPid = gtracker_track:open(Track),
   gtracker_track:clear(TrackPid),

   TrackPid ! #coord{lat = 123.1, lon = 321.12, speed = 45, timestamp = now()},
   TrackPid ! #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()},
   TrackPid ! #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()},
   TrackPid ! #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()},

   gtracker_track:close(TrackPid),

   timer:sleep(200),

   TrackPid1 = gtracker_track:open(Track),
   TrackPid2 = gtracker_track:open(Track),

   Res1 = lists:sort(gtracker_track:get_coords(TrackPid1)),
   Res2 = lists:sort(gtracker_track:get_coords(TrackPid2)),
   ?assertEqual(Res1, Res2),

   gtracker_track:close(TrackPid1),
   gtracker_track:close(TrackPid2).

-endif.
