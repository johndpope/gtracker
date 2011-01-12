-module(gtracker_track_pub).

-include("common_recs.hrl").
-include("common_defs.hrl").

-export([open/3, close/1, store/2, clear/1, set_owner/2, set_subscribers/2, coords/1, stat/1]).

open(Db, Track, CalcSpeed) ->
   rpc:call(Track#track.node, gtracker_track, open, [Db, Track, self(), CalcSpeed]).

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
