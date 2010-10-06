-module(gtracker_edb).

-behaviour(mds_gen_server).

-export([start/1, stop/0, on_start/1, on_stop/2, on_msg/3, on_amsg/2, on_info/2]).

-import(mds_utils, [get_param/2, get_param/3]).
-import(gtracker_common, [gen_dev_name/0, ints_to_float/2]).

-include("fields.hrl").

-define(MOD, {global, ?MODULE}).
-define(DEF_FAILOVER_PERIOD, 10).
-define(DEF_TABLE_DIR, ".").
-define(DEF_KEY_POS, 2).

-record(state, {settings = []}).
-record(device, {id, alias = undef, when_registered, is_online = true, num_tracks = 0}).
-record(track, {id, 'name' = undef, started = undef, stopped = undef, tbl_name}). % id is a compound key like {device, Int()}
-record(active_track, {dev_name, track_id, table}).
-record(coord, {timestamp, lat, lon, when_inserted}).

%=======================================================================================================================
%  public exports
%=======================================================================================================================
% Opts = [Option]
% Option = {dump_timeout, Int()} | {auto_unload, Int()} | {failover_time, Int()}
%  dump_timeout   - see gtracker_table for details
%  auto_unload    - see gtracker_table for details
%  failover_time  - period of time in seconds after what track become closed, else this track will be reopened
start(Opts) ->
   mds_gen_server:start(?MOD, Opts).

stop() ->
   mds_gen_server:stop(?MOD).

%=======================================================================================================================
%  callbacks
%=======================================================================================================================
on_start(Opts) ->
   crypto:start(),
   ServerOpts = get_param(mds_server, Opts),
   SelfOpts = get_param(self, Opts, []),
   WorkingDir = filename:join(get_param(root_dir, ServerOpts), get_param(table_dir, SelfOpts, ?DEF_TABLE_DIR)),
   mnesia:start(),
   Res = (catch mnesia:table_info(device, all)),
   case Res of
      {'EXIT',{aborted,{no_exists,device,all}}} ->
         create_schema();
      _ -> ok
   end,
   log(info, "Mnesia started."),
   {ok, #state{settings = [{working_dir, WorkingDir}, {key_pos, ?DEF_KEY_POS}, SelfOpts]}}.

on_stop(Reason, _State) ->
   crypto:stop(),
   mnesia:stop(),
   log(info, "Mnesia stopped."),
   log(info, "Stopped <~p>.", [Reason]),
   ok.

on_msg(stop, _From, State) ->
   {stop, normal, stopped, State};

%=======================================================================================================================
%  get new device name
%=======================================================================================================================
on_msg(get_device, _From, State) ->
   F = fun(Fun) ->
         NewDevName = gen_dev_name(),
         log(debug, "Device ~p generated.", [NewDevName]),
         case mnesia:dirty_read(device, NewDevName) of
            [] ->
               mnesia:dirty_write(#device{id = NewDevName, when_registered = now()}),
               NewDevName;
            [_] ->
               Fun(Fun)
         end
      end,
   try F(F) of
      DevName -> {reply, DevName, State}
   catch
      _:Err ->
         log(error, "get_device/0 failed: ~p", [Err]),
         {reply, error, State}
   end;

%=======================================================================================================================
%  check existing device name
%=======================================================================================================================
on_msg({get_device, DevName}, _From, State) ->
   Res =(catch mnesia:dirty_read(device, DevName)),
   case Res of
      [{device, DevName, _, _, true, _}] ->
         {reply, {DevName, online}, State};
      [{device, DevName, _, _, false, _}] ->
         {reply, {DevName, offline}, State};
      [] ->
         {reply, not_found, State};
      Err ->
         log(error, "get_device/1 failed: ~p", [Err]),
         {reply, error, State}
   end;

%=======================================================================================================================
%  stop track
%=======================================================================================================================
on_msg({stop_track, DevName}, _From, State) ->
   stop_track(DevName),
   {reply, stopped, State};

%=======================================================================================================================
%  terminator
%=======================================================================================================================
on_msg(Msg, From, State) ->
   log(error, "Unknown msg ~p has received from ~p.", [Msg, From]),
   {reply, unknown_msg, State}.

%=======================================================================================================================
%  store coordinate
%=======================================================================================================================
on_amsg({coord, first, DevName, {_Lat, _Lon, Timestamp} = NewCoord}, State) ->
   TrackRef = get_track(DevName, Timestamp, State#state.settings),
   Coord = tuple2coord(NewCoord),
   case gtracker_table:insert(TrackRef, Coord) of
      ok ->
         log(debug, "Coordinate ~p was inserted.", [Coord]);
      Other ->
         log(error, "Coordinate ~p insertion failed. Error = ~p", [Coord, Other])
   end,
   {noreply, State};

on_amsg({coord, DevName, {_Lat, _Lon, Timestamp} = NewCoord}, State) ->
   TrackRef = get_track(DevName, Timestamp, State#state.settings),
   Coord = tuple2coord(NewCoord),
   case gtracker_table:insert(TrackRef, Coord) of
      ok ->
         log(debug, "Coordinate ~p was inserted.", [Coord]);
      Other ->
         log(error, "Coordinate ~p insertion failed. Error = ~p", [Coord, Other])
   end,
   {noreply, State};

%=======================================================================================================================
%  set device online
%=======================================================================================================================
on_amsg({online, DevName}, State) ->
   Res =(catch mnesia:dirty_read(device, DevName)),
   case Res of
      [] ->
         {noreply, State};
      [R] ->
         mnesia:dirty_write(R#device{is_online = true}),
         {noreply, State};
      Err ->
         log(error, "Set online failed: ~p", [Err]),
         {noreply, State}
   end;

%=======================================================================================================================
%  set device offline
%=======================================================================================================================
on_amsg({offline, DevName}, State) ->
   Res =(catch mnesia:dirty_read(device, DevName)),
   case Res of
      [] ->
         {noreply, State};
      [R] ->
         mnesia:dirty_write(R#device{is_online = false}),
         {noreply, State};
      Err ->
         log(error, "Set offline failed: ~p", [Err]),
         {noreply, State}
   end;

%=======================================================================================================================
%  change device name
%=======================================================================================================================
on_amsg({change_dev_name, _OldDevName, _NewDevName}, State) ->
   {noreply, State};

on_amsg(Msg, State) ->
   log(error, "Unknown async message ~p.", [Msg]),
   {noreply, State}.

on_info(Msg, State) ->
   log(error, "Unknown info message ~p.", [Msg]),
   {noreply, State}.

%=======================================================================================================================
%  log helpers
%=======================================================================================================================
log(LogLevel, Format, Data) ->
   mds_gen_server:log(?MODULE, LogLevel, Format, Data).

log(LogLevel, Text) ->
   mds_gen_server:log(?MODULE, LogLevel, Text).

tuple2coord({Lat, Lon, TimeStamp}) ->
   #coord{timestamp = TimeStamp, lat = Lat, lon = Lon, when_inserted = now()}.

%=======================================================================================================================
%  tools
%=======================================================================================================================
create_schema() ->
   mnesia:stop(),
   mnesia:create_schema([]),
   mnesia:start(),
   mnesia:create_table(device, [{disc_copies, [node()]}, {type, ordered_set}, {attributes, record_info(fields, device)}]),
   mnesia:create_table(track, [{disc_copies, [node()]}, {type, ordered_set}, {attributes, record_info(fields, track)}]),
   mnesia:create_table(active_track, [{type, ordered_set}, {attributes, record_info(fields, active_track)}]).

stop_track(DevName) ->
   case mnesia:dirty_read(active_track, DevName) of
      [] ->
         case mnesia:dirty_match_object(track, {track, {DevName, '_'}, '_', '_', undef, '_'}) of
            [#track{id = Id} = Track | []] ->
               mnesia:dirty_write(Track#track{stopped = now()}),
               log(warning, "Track {~p, ~p} was stopped, but it was non-active.", [DevName, Id]),
               ok;
            [#track{id = Id} = Track | _Rest] ->
               mnesia:dirty_write(Track#track{stopped = now()}),
               log(warning, "Track {~p, ~p} was stopped, but it was non-active.", [DevName, Id]),
               stop_track(DevName);
           [] ->
              log(warning, "Track was not stopped. Client ~p doesn't have opened tracks.", [DevName]),
              no_tracks
         end;
      [#active_track{dev_name = DevName, track_id = TrackId, table = T}] ->
         F = fun() ->
               [Track] = mnesia:read(track, {DevName, TrackId}),
               mnesia:write(Track#track{stopped = now()}),
               mnesia:delete({active_track, DevName})
             end,
         case mnesia:transaction(F) of
            {atomic, _} ->
                log(info, "Track {~p, ~p} was stopped", [DevName, TrackId]);
            {aborted, Reason} ->
               log(error, "Track {~p, ~p} stopping error for: ~p", [DevName, TrackId, Reason])
         end,
         gtracker_table:close(T),
      ok
   end.

get_track(DevName, Timestamp, Settings) ->
   FPeriod = get_param(failover_period, Settings, ?DEF_FAILOVER_PERIOD),
   case mnesia:dirty_read(active_track, DevName) of
      [] ->
         [{device, DevName, _, _, _, NumTracks}] = mnesia:dirty_read(device, DevName),
         case mnesia:dirty_read(track, {DevName, NumTracks}) of
            [{track, {DevName, NumTracks}, _, _, _, TableName} = Track] ->
               Table = gtracker_table:open(erlang:list_to_atom(TableName), Settings, fun log/3),
               case gtracker_table:last(Table) of
                  [] -> % reuse empty table
                     mnesia:dirty_write(Track#track{started = Now, stopped = undef}),
                     mnesia:dirty_write(#active_track{dev_name = DevName, track_id = NumTracks, table = Table}),
                     log(info, "Track {~p, ~p} is empty. Will be reused.", [DevName, NumTracks]),
                     Table;
                  [{coord, TS1, _, _, _}] ->
                     case calendar:datetime_to_gregorian_seconds()
                     when  < FPeriod ->
                        mnesia:dirty_write(Track#track{stopped = undef}),
                        mnesia:dirty_write(#active_track{dev_name = DevName, track_id = NumTracks, table = Table}),
                        log(info, "Track {~p, ~p} within failover period. Will be reused.", [DevName, NumTracks]),
                        Table;
                  [{coord, TS2, _, _, _}] ->
                     mnesia:dirty_write(Track#track{stopped = TS2}),
                     gtracker_table:close(Table),
                     create_active_track(DevName, Timestamp, Settings)
               end;
            _ ->
               create_active_track(DevName, Timestamp, Settings)
         end;
      [{active_track, DevName, _TrackId, Table}] ->
         Table
   end.

create_active_track(DevName, Timestamp, Settings) ->
   F = fun() ->
         [Device] = mnesia:read(device, DevName),
         TrackId = Device#device.num_tracks + 1,
         TableName = DevName ++ "_" ++ integer_to_list(TrackId),
         Table = gtracker_table:open(list_to_atom(TableName), Settings, fun log/3),  % TODO: pass parameters
         mnesia:write(Device#device{num_tracks = TrackId}),
         mnesia:write(#track{id = {DevName, TrackId}, started = Timestamp, tbl_name = TableName}),
         mnesia:write(#active_track{dev_name = DevName, track_id = TrackId, table = Table}),
         Table
      end,
   {atomic, TableRef} = mnesia:transaction(F),
   TableRef.

%=======================================================================================================================
%  unit testing facilities
%=======================================================================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

test_start_edb() ->
   start([
           {mds_server, [{root_dir, "/tmp"}]},
           {mds_logger, [{log_level, debug}]},
           {self, []}
        ]),
   wait_for_mnesia().

wait_for_mnesia() ->
   case ( catch mnesia:system_info(tables) ) of
      {'EXIT', {aborted, {node_not_running, _}}} ->
         timer:sleep(5),
         wait_for_mnesia();
      [schema] ->
         time:sleep(5),
         wait_for_mnesia();
      _ ->
         ok
   end.

wait_logger_to_stop() ->
   case lists:any(fun(X) -> X =:= gtracker_edb_logger end, registered()) of
      true ->
         timer:sleep(5),
         wait_logger_to_stop();
      false ->
         ok
   end.

test_stop_edb() ->
   stop(),
   wait_logger_to_stop().

test_clear_edb() ->
   ok = mnesia:delete_schema([node()]).

test_now() ->
   calendar:now_to_datetime(erlang:now()).

%% ==================== unit tests ========================= %%

tuple2coord_test() ->
   Timestamp = {1271, 95363, 0},
   Coord = {37.123456, 55.345678, Timestamp},
   ?assertMatch({coord, Timestamp, 37.123456, 55.345678, _Now}, tuple2coord(Coord)).

start_in_shell_test() ->
  test_start_edb(),
  test_stop_edb(),
  test_clear_edb().

get_device_test() ->
  test_start_edb(),
  DevName = mds_gen_server:call(?MOD, get_device),
  ?assert(is_list(DevName)),
  ?assert(length(DevName) =:= 12),
  test_stop_edb(),
  test_clear_edb().

get_device2_test() ->
  test_start_edb(),
  DevName = mds_gen_server:call(?MOD, get_device),
  ?assert(is_list(DevName)),
  ?assert(length(DevName) =:= 12),
  ?assertEqual({DevName, online}, mds_gen_server:call(?MOD, {get_device, DevName})),
  ?assertEqual(not_found, mds_gen_server:call(?MOD, {get_device, "12345QWERTQA"})),
  test_stop_edb(),
  test_clear_edb().

set_device_off_on_test() ->
  test_start_edb(),
  DevName = mds_gen_server:call(?MOD, get_device),
  ?assertEqual({DevName, online}, mds_gen_server:call(?MOD, {get_device, DevName})),
  mds_gen_server:cast(?MOD, {offline, DevName}),
  ?assertEqual({DevName, offline}, mds_gen_server:call(?MOD, {get_device, DevName})),
  mds_gen_server:cast(?MOD, {online, DevName}),
  ?assertEqual({DevName, online}, mds_gen_server:call(?MOD, {get_device, DevName})),
  test_stop_edb(),
  test_clear_edb().

create_active_track_test() ->
  test_start_edb(),
  Settings = [{working_dir, "/tmp"}, {key_pos, 2}],
  DevName = mds_gen_server:call(?MOD, get_device),
  ?assertEqual({DevName, online}, mds_gen_server:call(?MOD, {get_device, DevName})), % generate new DevName
  Now = test_now(),
  TableRef = create_active_track(DevName, Now, Settings),  % create new empty track with started = now()
  ?assertMatch([{device, DevName, undef, _, true, 1}], mnesia:dirty_read(device, DevName)),
  TableName = DevName ++ "_1",
  ?assertMatch([{track, {DevName, 1}, undef, Now, undef, TableName}], mnesia:dirty_read(track, {DevName, 1})),
  ?assertMatch([{active_track, DevName, 1, TableRef}], mnesia:dirty_read(active_track, DevName)),
  TableRef2 = create_active_track(DevName, Now, Settings),
  ?assertMatch([{device, DevName, undef, _, true, 2}], mnesia:dirty_read(device, DevName)),
  TableName2 = DevName ++ "_2",
  ?assertMatch([{track, {DevName, 2}, undef, Now, undef, TableName2}], mnesia:dirty_read(track, {DevName, 2})),
  ?assertMatch([{active_track, DevName, 2, TableRef2}], mnesia:dirty_read(active_track, DevName)),
  test_stop_edb(),
  test_clear_edb().

get_track_test() ->
 test_start_edb(),
 Settings = [{working_dir, "/tmp"}, {failover_period, 2}, {key_pos, 2}],
 Now = test_now(),
 DevName = mds_gen_server:call(?MOD, get_device),
 ?assertEqual({DevName, online}, mds_gen_server:call(?MOD, {get_device, DevName})), % generate new DevName
 TrackRef = get_track(DevName, Now, Settings),
 ?assertEqual(TrackRef, get_track(DevName, Now, Settings)), % should return the same track.
 ?assertMatch([{device, DevName, _, _, true, 1}], mnesia:dirty_read(device, DevName)),
{atomic, ok} = mnesia:clear_table(active_track),
 ?assertEqual([], mnesia:dirty_read(active_track, DevName)),
 gtracker_table:close(TrackRef),
 % now we have one empty, non-active, unclosed track. It should be reopened
 ?assertEqual(TrackRef, get_track(DevName, Now, Settings)),
 ?assertMatch([{device, DevName, _, _, true, 1}], mnesia:dirty_read(device, DevName)),
 gtracker_table:insert(TrackRef, #coord{timestamp = Now, lat = 37.123456, lon = 55.345678, when_inserted =
       now()}),
 {atomic, ok} = mnesia:clear_table(active_track),
 gtracker_table:close(TrackRef),
 % now we have non-empty, non-active, unclosed track. It should be reopened, because we hit into failover period
 ?assertEqual(TrackRef, get_track(DevName, Now, Settings)), % should return the same track.
 ?assertMatch([{device, DevName, _, _, true, 1}], mnesia:dirty_read(device, DevName)),
 {atomic, ok} = mnesia:clear_table(active_track),
 gtracker_table:close(TrackRef),
 timer:sleep(3000),
 % now we have non-empty, non-active, unclosed track. It should be closed and new one should be opened,
 % because failover period is over
 TrackRef2 = get_track(DevName, test_now(), Settings), % should return the same track.
 ?assertMatch([{device, DevName, _, _, true, 2}], mnesia:dirty_read(device, DevName)),
 ?assertMatch([{track, {DevName, 1}, _, {_, _, _}, {_, _, _}, _}], mnesia:dirty_read(track, {DevName, 1})),
 test_stop_edb(),
 test_clear_edb().

stop_track_test() ->
 test_start_edb(),
 Settings = [{working_dir, "/tmp"}, {failover_period, 0}, {key_pos, 2}],
 Now = test_now(),
 DevName = mds_gen_server:call(?MOD, get_device),
 % DevName doens't have opened tracks. no_tracks should be returned
 ?assertEqual(no_tracks, stop_track(DevName)),
 TrackRef = get_track(DevName, Now, Settings),
 ?assertMatch([{device, DevName, _, _, true, 1}], mnesia:dirty_read(device, DevName)),
 ?assertMatch([{active_track, DevName, 1, TrackRef}], mnesia:dirty_read(active_track, DevName)),
 ?assertMatch([{track, {DevName, 1}, _, {_, _, _}, undef, _}], mnesia:dirty_read(track, {DevName, 1})),
 stop_track(DevName),
 ?assertMatch([], mnesia:dirty_read(active_track, DevName)),
 ?assertMatch([{track, {DevName, 1}, _, {_, _, _}, {_, _, _}, _}], mnesia:dirty_read(track, {DevName, 1})),
 % create new track, remove it from active_track table, but not stop it.
 TrackRef = get_track(DevName, Now, Settings),
 ?assertMatch([{active_track, DevName, 1, TrackRef}], mnesia:dirty_read(active_track, DevName)),
 ?assertMatch([{track, {DevName, 1}, _, {_, _, _}, undef, _}], mnesia:dirty_read(track, {DevName, 1})),
 {atomic, ok} = mnesia:clear_table(active_track),
 stop_track(DevName),
 ?assertMatch([{track, {DevName, 1}, _, {_, _, _}, {_, _, _}, _}], mnesia:dirty_read(track, {DevName, 1})),
 test_stop_edb(),
 test_clear_edb().

full_test() ->
   erlang:trace(new, true, [all]),
   test_start_edb(),
%   Settings = [{working_dir, "/tmp"}, {failover_period, 0}, {key_pos, 2}],
   Now = test_now(),
   DevName = mds_gen_server:call(?MOD, get_device),
   ?assert(is_list(DevName)),
   ?assert(length(DevName) =:= 12),
   ?assertEqual({DevName, online}, mds_gen_server:call(?MOD, {get_device, DevName})),
   {MSec, Sec, _} = Now = test_now(),
   mds_gen_server:cast(?MOD, {coord, first, DevName, {37.123456, 55.345678, Now}}),
   timer:sleep(1000),
   [{active_track, DevName, 1, TrackRef}] = mnesia:dirty_read(active_track, DevName),
   ?assertMatch([{coord, Now, 37.123456, 55.345678, _}], gtracker_table:lookup(TrackRef, Now)),
   mds_gen_server:cast(?MOD, {coord, DevName, {37.123457, 55.345679, {MSec, Sec + 1, 0}}}),
   mds_gen_server:cast(?MOD, {coord, DevName, {37.123458, 55.345680, {MSec, Sec + 2, 0}}}),
   mds_gen_server:cast(?MOD, {coord, DevName, {37.123459, 55.345681, {MSec, Sec + 3, 0}}}),
   mds_gen_server:cast(?MOD, {coord, DevName, {37.123460, 55.345682, {MSec, Sec + 4, 0}}}),
   mds_gen_server:cast(?MOD, {coord, DevName, {37.123461, 55.345683, {MSec, Sec + 5, 0}}}),
   timer:sleep(1000),
   ?assertEqual(6, gtracker_table:info(TrackRef, size)),
   test_stop_edb(),
   test_clear_edb().

-endif.
