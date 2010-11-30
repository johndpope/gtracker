-module(gtracker_track).

-include("common_recs.hrl").

-export([start/2, start/3, stop/1, init/3, loop/2, clear/1, close/1, get_coords/1]).

start(Track, DbRef) ->
   rpc:call(Track#track.node, gtracker_track, start, [Track#track.id, Track#track.path, DbRef]).

stop(TrackPid) ->
   TrackPid ! stop.

start(TrackId, Path, DbRef) ->
   spawn_link(fun() -> init(TrackId, Path, DbRef) end).

init(TrackId, Path, DbRef) ->
   {ok, Ref} = dets:open_file(TrackId, [{auto_save, 1000}, {file, Path}, {keypos, 5}]),
   loop(Ref, DbRef).

loop(Name, DbRef) ->
   receive
      Coord when is_record(Coord, coord) ->
         ok = dets:insert(Name, Coord),
         loop(Name, DbRef);
      {get_coords, Peer} ->
         Coords = dets:select(Name, [{'_', [], ['$_']}]),
         Peer ! {reply, Coords},
         loop(Name, DbRef);
      clear ->
         dets:delete_all_objects(Name),
         loop(Name, DbRef);
      stop ->
         Stat = calc_stat(Name),
         gen_server:cast(DbRef, {track_stat, Name, Stat}),
         dets:close(Name);
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

calc_stat(Name) ->
   ok.
%   Coords = dets:select(Name, [{#coord{_='_'}}, [], ['$_']]),
%   SortedCoords = lists:sort(fun(#coord{timestamp = A}, #coord{timestamp = B}) -> A <= B end, Coord),
%   lists:foldl(fun(#coord{}))


%=======================================================================================================================
%  unit testing facilities
%=======================================================================================================================
%-ifdef(TEST).
%-include_lib("eunit/include/eunit.hrl").

%track_test() ->
%   TrackPid = gtracker_track:start(tmp, self(), "/tmp/tmp.dets"),
%   gtracker_track:clear(TrackPid),

%   Coord1 = #coord{lat = 123.1, lon = 321.12, speed = 45, timestamp = now()},
%   TrackPid ! Coord1,

%   Coord2 = #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()},
%   TrackPid ! Coord2,

%   Coord3 = #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()},
%   TrackPid ! Coord3,

%   Coord4 = #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()},
%   TrackPid ! Coord4,

%   ?assertEqual(lists:sort([Coord1, Coord2, Coord3, Coord4]), lists:sort(get_coords(TrackPid))),

%   gtracker_track:clear(TrackPid),

%   ?assertEqual([], lists:sort(get_coords(TrackPid))),

%   gtracker_track:close(TrackPid).

%track2_test() ->
%   TrackPid = gtracker_track:start(tmp, "/tmp/tmp1.dets"),
%   gtracker_track:clear(TrackPid),

%   TrackPid ! #coord{lat = 123.1, lon = 321.12, speed = 45, timestamp = now()},
%   TrackPid ! #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()},
%   TrackPid ! #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()},
%   TrackPid ! #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp = now()},

%   gtracker_track:close(TrackPid),

%   timer:sleep(200),

%   TrackPid1 = gtracker_track:start(tmp, "/tmp/tmp1.dets"),
%   TrackPid2 = gtracker_track:start(tmp, "/tmp/tmp1.dets"),

%   Res1 = lists:sort(gtracker_track:get_coords(TrackPid1)),
%   Res2 = lists:sort(gtracker_track:get_coords(TrackPid2)),
%   ?assertEqual(Res1, Res2),

%   gtracker_track:close(TrackPid1),
%   gtracker_track:close(TrackPid2).

%-endif.
