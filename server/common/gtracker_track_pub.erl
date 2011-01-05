-module(gtracker_track_pub).

-include("common_recs.hrl").
-include("common_defs.hrl").

-export([open/3, close/1, store/2, clear/1, set_owner/2, set_subscribers/2, coords/1, stat/1]).

open(Db, Track, CalcSpeed) ->
   Pid = rpc:call(Track#track.node, gtracker_track, open, [Db, Track, self(), CalcSpeed]),
   Track#track{pid = Pid}.

close(#track{pid = Pid}) when is_pid(Pid) == false ->
   {error, wrong_pid, [Pid]};
close(#track{pid = Pid}) ->
   Pid ! {close, self()},
   ok.

store(#track{pid = Pid}, _) when is_pid(Pid) == false ->
   {error, wrong_track_pid, [Pid]};
store(#track{pid = Pid}, Coord) ->
   Pid ! Coord,
   ok.

clear(#track{pid = Pid}) when is_pid(Pid) == false ->
   {error, wrong_track_pid, [Pid]};
clear(#track{pid = Pid}) ->
   Pid ! clear,
   ok.

set_owner(_, undef) ->
   {error, owner_pid_is_wrong, [undef]};
set_owner(#track{pid = Pid}, _OwnerPid) when is_pid(Pid) == false ->
   {error, wrong_track_pid, [Pid]};
set_owner(#track{pid = Pid}, OwnerPid) ->
   Pid ! {owner, OwnerPid},
   ok.

set_subscribers(_, undef) ->
   {error, subs_pid_is_wrong, [undef]};
set_subscribers(#track{pid = Pid}, _) when is_pid(Pid) == false ->
   {error, wrong_track_pid, [Pid]};
set_subscribers(#track{pid = Pid}, Subscribers) ->
   Pid ! {subscribers, Subscribers}.

stat(#track{id = TrackId, node = Node, path = Path}) ->
   {ok, Ref} = rpc:call(Node, dets, open_file, [TrackId, ?track_open_args]),
   UnsortedCoords = dets:select(Ref, [{'_', [], ['$_']}]),
   Coords = lists:keysort(?FieldId(coord, timestamp), UnsortedCoords),
   {Length, AvgSpeed, _} = lists:foldl(
      fun(#coord{speed = S, distance = D}, {SumD, AvgSpeed, Cnt}) ->
         NewAvgSpeed = (AvgSpeed * Cnt  + S) / (Cnt + 1),
         {SumD + D, NewAvgSpeed, Cnt + 1}
      end,
      {0, 0.0, 0}, Coords),
   NumCoords = erlang:length(Coords),
   Start = if length(Coords) > 0 -> (erlang:hd(Coords))#coord.timestamp; true -> undef end,
   Stop =  if length(Coords) > 0 -> (lists:last(Coords))#coord.timestamp; true -> undef end,
   rpc:call(Node, dets, close, [Ref]),
   {TrackId, Start, Stop, NumCoords, Length, AvgSpeed}.

coords(#track{id = TrackId, node = Node, path = Path}) ->
   {ok, Ref} = rpc:call(Node, dets, open_file, [TrackId, ?track_open_args]),
   UnsortedCoords = dets:select(Ref, [{'_', [], ['$_']}]),
   rpc:call(Node, dets, close, [Ref]),
   lists:keysort(?FieldId(coord, timestamp), UnsortedCoords).

%=======================================================================================================================
%  unit testing facilities
%=======================================================================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

track_test() ->
   TmpTrack = #track{id = 'track_1', path = "/tmp/track_1"},
   Track = gtracker_track_pub:open(?db_ref, TmpTrack, true),
   ?assertEqual(Track, gtracker_track_pub:open(?db_ref, TmpTrack, true)),
   gtracker_track_pub:close(Track).

track1_test() ->
   TmpTrack = #track{id = 'track_2', path = "/tmp/track_2"},
   Track = gtracker_track_pub:open(?db_ref, TmpTrack, true),
   gtracker_track_pub:clear(Track),

   Timestamp = calendar:local_time(),
   Coord1 = #coord{lat = 123.1, lon = 321.12, speed = 45, timestamp = Timestamp},
   gtracker_track_pub:store(Track, Coord1),

   Coord2 = #coord{lat = 123.2, lon = 321.3, speed = 45,
      timestamp = setelement(2, Timestamp, calendar:seconds_to_time(calendar:time_to_seconds(element(2, Timestamp)) + 1))},
   gtracker_track_pub:store(Track, Coord2),

   Coord3 = #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp =
      setelement(2, Timestamp, calendar:seconds_to_time(calendar:time_to_seconds(element(2, Timestamp)) + 2))},
   gtracker_track_pub:store(Track, Coord3),

   Coord4 = #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp =
      setelement(2, Timestamp, calendar:seconds_to_time(calendar:time_to_seconds(element(2, Timestamp)) + 3))},
   gtracker_track_pub:store(Track, Coord4),

   timer:sleep(1000),

   ?assertEqual(lists:sort([Coord1, Coord2, Coord3, Coord4]), gtracker_track_pub:coords(Track)),

   gtracker_track_pub:clear(Track),

   ?assertEqual([], lists:sort(coords(Track))),

   gtracker_track_pub:close(Track).

track2_test() ->
  TmpTrack = #track{id = 'track_3', path = "/tmp/track_3"},
  Track = gtracker_track_pub:open(?db_ref, TmpTrack, true),
  gtracker_track_pub:clear(Track),

  Timestamp = calendar:local_time(),
  gtracker_track_pub:store(Track, #coord{lat = 123.1, lon = 321.12, speed = 45, timestamp = Timestamp}),
  gtracker_track_pub:store(Track, #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp =
        setelement(2, Timestamp, calendar:seconds_to_time(calendar:time_to_seconds(element(2, Timestamp)) + 1))}),
  gtracker_track_pub:store(Track, #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp =
        setelement(2, Timestamp, calendar:seconds_to_time(calendar:time_to_seconds(element(2, Timestamp)) + 2))}),
  gtracker_track_pub:store(Track, #coord{lat = 123.2, lon = 321.3, speed = 45, timestamp =
        setelement(2, Timestamp, calendar:seconds_to_time(calendar:time_to_seconds(element(2, Timestamp)) + 3))}),
  gtracker_track_pub:close(Track),

  timer:sleep(200),

  Track1 = gtracker_track_pub:open(?db_ref, Track, true),
  Track2 = gtracker_track_pub:open(?db_ref, Track, true),

  Res1 = lists:sort(gtracker_track_pub:coords(Track1)),
  Res2 = lists:sort(gtracker_track_pub:coords(Track2)),
  ?assertEqual(Res1, Res2),

  gtracker_track_pub:close(Track1),
  gtracker_track_pub:close(Track2).

track3_test() ->
  TmpTrack = #track{id = 'track_4', path = "/tmp/track_4"},
  Track = gtracker_track_pub:open(?db_ref, TmpTrack, true),
  gtracker_track_pub:clear(Track),

  Timestamp = calendar:local_time(),
  gtracker_track_pub:store(Track, #coord{lat = 123.1, lon = 321.12, distance = 10, speed = 45, timestamp = Timestamp}),
  gtracker_track_pub:store(Track, #coord{lat = 123.2, lon = 321.3, distance = 10, speed = 45, timestamp =
     setelement(2, Timestamp, calendar:seconds_to_time(calendar:time_to_seconds(element(2, Timestamp)) + 1))}),
  gtracker_track_pub:store(Track, #coord{lat = 123.2, lon = 321.3, distance = 10, speed = 45, timestamp =
     setelement(2, Timestamp, calendar:seconds_to_time(calendar:time_to_seconds(element(2, Timestamp)) + 2))}),
  Timestamp4 = setelement(2, Timestamp, calendar:seconds_to_time(calendar:time_to_seconds(element(2, Timestamp)) + 3)),
  gtracker_track_pub:store(Track, #coord{lat = 123.2, lon = 321.3, distance = 10, speed = 45, timestamp = Timestamp4}),

  timer:sleep(1000),

  ?assertEqual({'track_4', Timestamp, Timestamp4, 4, 40, 45.0}, gtracker_track_pub:stat(Track)).

track4_test() ->
  TmpTrack = #track{id = 'track_5', path = "/tmp/track_5"},
  Track = gtracker_track_pub:open(?db_ref, TmpTrack, true),
  gtracker_track_pub:clear(Track),

  ?assertEqual({'track_5', undef, undef, 0, 0, 0.0}, gtracker_track_pub:stat(Track)).

-endif.
