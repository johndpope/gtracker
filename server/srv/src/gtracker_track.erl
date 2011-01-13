-module(gtracker_track).

-include("common_recs.hrl").
-include("common_defs.hrl").

-include_lib("eunit/include/eunit.hrl").

-export([open/4, init/5, loop/1]).

-record(state, {db, track, subs = [], ref, owner, calc_speed = false}).

open(Db, Track, Owner, CalcSpeed) ->
   case erlang:whereis(Track#track.id) of
      undefined ->
         Self = self(),
         Pid = spawn_link(fun() -> init(Self, Db, Track, Owner, CalcSpeed) end),
         receive
            Err = {error, _, _} ->
               Err;
            {ok, NewTrack} ->
               register(NewTrack#track.id, Pid),
               {ok, NewTrack}
         end;
      Pid ->
         Track#track{pid = Pid}
   end.

init(Creator, Db, Track = #track{id = Id, path = Path}, Owner, CalcSpeed) ->
   F = fun() ->
      process_flag(trap_exit, true),
      {ok, Ref} = dets:open_file(Id, ?track_open_args),
      NewTrack = load_track_stat(Ref, Track, CalcSpeed),
      NewTrack2 = NewTrack#track{pid = self()},
      {ok, NewTrack2} = gtracker_pub:update(Db, NewTrack#track{pid = self()}, [avg_speed, length, coord_count, start,
            stop, pid], ?MAX_CALL_TIMEOUT),
      gen_server:cast(Db, {updated, NewTrack2}),
      erlang:start_timer(60000, self(), update_db),
      Creator ! {ok, NewTrack2},
      loop(#state{db = Db, track = NewTrack, ref = Ref, owner = Owner, calc_speed = CalcSpeed})
   end,
   try F()
   catch
      _:Err ->
         gen_server:call(Db, {failed_node, node()}, ?MAX_CALL_TIMEOUT),
         Creator ! {error, Err, [Db, Track, Owner, CalcSpeed]}
   end.

loop(State = #state{db = Db, track = Track, subs = S, ref = Ref, owner = Owner, calc_speed = CalcSpeed}) ->
   receive
      {timeout, _, update_db} ->
         gen_server:cast(Db, {updated, Track}),
         loop(State);
      Coord when is_record(Coord, coord) ->
         ok = dets:insert(Ref, Coord),
         NewS = gtracker_common:send2subs(S, Coord),
         NewTrack = update_track_stat(Track, Coord, CalcSpeed),
         NewS2 = gtracker_common:send2subs(NewS, {updated, NewTrack}),
         loop(State#state{subs = NewS2, track = NewTrack});
      {owner, NewOwner} ->
         error_logger:info_msg("The owner ~p has  been changed to ~p~n", [Owner, NewOwner]),
         link(NewOwner),
         loop(State#state{owner = NewOwner});
      {subscribers, NewSubs} ->
         loop(State#state{subs = NewSubs});
      clear ->
         dets:delete_all_objects(Ref),
         loop(State);
      {close, Peer} when Peer == Owner ->
         error_logger:info_msg("~p: owner ~p want me to close. Closing...~n", [Track#track.id, Owner]),
         dets:close(Ref),
         gen_server:cast(Db, {closed, Track});
      {close, _} ->
         loop(State);
      {'EXIT', Pid, Reason} when Pid =/= Owner ->
         error_logger:info_msg("~p: looks like old owner ~p exited with reason '~p'. Ignored.~n",
            [Track#track.id, Pid, Reason]),
         loop(State);
      {'EXIT', Owner, Reason} ->
         error_logger:info_msg("~p: owner ~p exited with reason '~p'. Closing...~n", [Track#track.id, Owner, Reason]),
         dets:close(Ref),
         gen_server:cast(Db, {closed, Track});
      {updated, #track{name = NewName}} ->
         loop(#state{track = Track#track{name = NewName}});
      Msg ->
         error_logger:error_msg("~p: Unknown message ~p was ignored~n", [Track#track.id, Msg]),
         loop(State)
   end.

-include_lib("eunit/include/eunit.hrl").

update_track_stat(Track = #track{start = undef}, C = #coord{timestamp = TS}, CalcSpeed) ->
   update_track_stat(Track#track{start = TS}, C, CalcSpeed);
update_track_stat(Track = #track{avg_speed = AvgSpeed, length = TrackLength, coord_count = Cnt, start = StartTS},
   #coord{speed = Speed, distance = D, timestamp = TS}, CalcSpeed) ->
   NewAvgSpeed =
   if (CalcSpeed == true) ->
      TsDiff = datetime_diff(TS, StartTS),
      if (TsDiff == 0) ->
         0;
      true ->
         (TrackLength + D) / TsDiff * 3.6
      end;
   true ->
      (AvgSpeed * Cnt + Speed) / (Cnt + 1) * 3.6
   end,
   Track#track{avg_speed = NewAvgSpeed, coord_count = Cnt + 1, length = TrackLength + D, stop = TS}.

load_track_stat(Ref) ->
   UnsortedCoords = dets:select(Ref, [{'_', [], ['$_']}]),
   Coords = lists:keysort(?FieldId(coord, timestamp), UnsortedCoords),
   {CumSpeed, Length, Cnt, Start, Stop} =
      lists:foldl(fun(#coord{speed = S, distance = D, timestamp = TS}, {AccS, AccD, AccCnt, undef, undef}) ->
                        {AccS + S, AccD + D, AccCnt + 1, TS, TS};
                     (#coord{speed = S, distance = D, timestamp = TS}, {AccS, AccD, AccCnt, Start, _}) ->
                        {AccS + S, AccD + D, AccCnt + 1, Start, TS}
                  end, {0, 0, 0, undef, undef}, Coords),
   case Cnt == 0 of
      true ->
         {0, Length, Cnt, Start, Stop};
      false ->
         {CumSpeed / Cnt * 3.6, Length, Cnt, Start, Stop}
      end.

load_track_stat(Ref, Track, CalcSpeed) ->
   {AvgSpeed, Length, Cnt, Start, Stop} = load_track_stat(Ref),
   if (CalcSpeed == true) ->
      TsDiff = datetime_diff(Stop, Start),
      if (TsDiff == 0) ->
         Track#track{start = Start, stop = Stop, length = Length, coord_count = Cnt, avg_speed = 0};
      true ->
         Track#track{start = Start, stop = Stop, length = Length, coord_count = Cnt, avg_speed = Length / TsDiff * 3.6}
      end;
   true ->
      Track#track{start = Start, stop = Stop, length = Length, coord_count = Cnt, avg_speed = AvgSpeed}
   end.

datetime_diff(T2, T1) when (T2 == undef) orelse (T1 == undef) ->
   0;
datetime_diff(T2, T1) ->
   calendar:datetime_to_gregorian_seconds(T2) - calendar:datetime_to_gregorian_seconds(T1).

%=======================================================================================================================
%  unit testing facilities
%=======================================================================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

load_track_stat_test() ->
  Path = "/tmp/track_1",
  {ok, Ref} = dets:open_file(track_1, ?track_open_args),
  dets:delete_all_objects(Ref),
  ?assertEqual({0, 0, 0, undef, undef}, load_track_stat(Ref)),
  Timestamp = calendar:local_time(),
  dets:insert(Ref, #coord{lat = 1, lon = 2, speed = 10, distance = 100, timestamp = Timestamp}),
  ?assertEqual({36.0, 100, 1, Timestamp, Timestamp}, load_track_stat(Ref)),
  Timestamp2 = setelement(2, Timestamp, calendar:seconds_to_time(calendar:time_to_seconds(element(2, Timestamp)) + 1)),
  dets:insert(Ref, #coord{lat = 1, lon = 2, speed = 20, distance = 200, timestamp = Timestamp2}),
  ?assertEqual({54.0, 300, 2, Timestamp, Timestamp2}, load_track_stat(Ref)).

load_track_stat2_test() ->
  Path = "/tmp/track_2",
  {ok, Ref} = dets:open_file(track_2, ?track_open_args),
  dets:delete_all_objects(Ref),
  ?assertEqual({0, 0, 0, undef, undef}, load_track_stat(Ref)),
  Timestamp = calendar:local_time(),
  dets:insert(Ref, #coord{lat = 1, lon = 2, speed = 10, distance = 100, timestamp = Timestamp}),
  ?assertEqual({36.0, 100, 1, Timestamp, Timestamp}, load_track_stat(Ref)),
  Timestamp2 = setelement(2, Timestamp, calendar:seconds_to_time(calendar:time_to_seconds(element(2, Timestamp)) + 1)),
  dets:insert(Ref, #coord{lat = 1, lon = 2, speed = 20, distance = 200, timestamp = Timestamp2}),
  ?assertEqual({54.0, 300, 2, Timestamp, Timestamp2}, load_track_stat(Ref)).

update_track_stat_test() ->
   Track = #track{id = 'track', dev_name = "SX6UAGDDCTUC",name = undef, pid = undef,
       node = 'node@node', status = opened, path = "/tmp/track", start = {{2010,6,7},{11,14,57}},
       stop = {{2010,6,7},{11,25,19}}, length = 6515.416756752921, avg_speed = 0.0, coord_count = 266},
   Coord = #coord{lat = 55.658434, lon = 37.787873, speed = 0, distance = 23.531633205356556, timestamp = {{2010,6,7},{11,25,20}}},
   NewTrack = update_track_stat(Track, Coord, true),
   ?assertEqual(37.78525554390016, NewTrack#track.avg_speed).


-endif.
