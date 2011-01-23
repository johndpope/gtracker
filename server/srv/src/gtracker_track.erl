-module(gtracker_track).

-behaviour(mds_gen_server).

-export([start/1, stop/1, on_start/1, on_stop/2, on_msg/3, on_amsg/2, on_info/2]).

-import(mds_utils, [get_param/2, get_param/3]).
-import(gtracker_common, [join_pg/2, leave_pg/2, send_metric/2]).

-include("common_defs.hrl").
-include("common_recs.hrl").

-record(state, {
      name,
      pg,
      mt,
      saver = undef,
      timer_ref = undef,
      metric_send_period = 0,
      coord_received = 0,
      active_tracks = 0
   }).
-record(owner, {pid, track_id}).

%=======================================================================================================================
%  public exports
%=======================================================================================================================
start(Opts) ->
   SelfOpts = get_param(self, Opts),
   ServName = {global, _} = get_param(name, SelfOpts), % should have {global, Atom()} format
   mds_gen_server:start(ServName, ?MODULE, Opts).

stop(State) ->
   mds_gen_server:stop(State#state.name).

%=======================================================================================================================
%  callbacks
%=======================================================================================================================
on_start(Opts) ->
   SelfOpts = get_param(self, Opts),
   ServName = get_param(name, SelfOpts),
   ProcGroup = get_param(group, SelfOpts, track),
   MetricSendPeriod = get_param(metric_send_period, SelfOpts, ?def_metric_send_period),
   Mt = get_param(mt, SelfOpts),
   mnesia_start(),
   process_flag(trap_exit, true),
   join_pg(ProcGroup, self()),
   SaverPid = spawn_link(fun() -> saver_loop(saver_name(ServName), MetricSendPeriod, now(), 0) end),
   {ok, TimerRef} = timer:send_interval(MetricSendPeriod, self(), send_metric),
   State = #state{
      name = ServName,
      pg = ProcGroup,
      mt = Mt,
      saver = SaverPid,
      metric_send_period = MetricSendPeriod,
      timer_ref = TimerRef},
   log(State, info, "Track started."),
   {ok, State}.

on_stop(Reason, State) ->
   timer:cancel(State#state.timer_ref),
   leave_pg(State#state.pg, self()),
   log(State, info, "Stopped <~p>.", [Reason]),
   ok.

on_msg(stop, _From, State) ->
   {stop, normal, stopped, State};

on_msg(Msg = name, _, State) ->
   log(State, debug, "~p", [Msg]),
   {reply, {ok, State#state.name}, State};

on_msg(Msg = process_info, _, State) ->
   log(State, debug, "~p", [Msg]),
   [{_, Size}] = process_info(self(), [message_queue_len]),
   {reply, Size, State};

on_msg(Msg = {close, TrackId}, _, State = #state{mt = Mt, active_tracks = AT}) ->
   log(State, debug, "~p", [Msg]),
   case mnesia:dirty_read(track_stat, TrackId) of
      [] ->
         {reply, {errror, no_such_track, [TrackId]}, State};
      [TrackStat] ->
         UpTrackStat = TrackStat#track_stat{subs = [], status = closed},
         gtracker_common:send2subs(TrackStat#track_stat.subs, UpTrackStat),
         mnesia:dirty_write(UpTrackStat),
         gen_server:cast(Mt, {closed, UpTrackStat}),
         {reply, ok, State#state{active_tracks = AT - 1}}
   end;

on_msg(Msg = {subscribers, TrackId, Subs}, _, State) ->
   log(State, debug, "~p", [Msg]),
   case mnesia:dirty_read(track_stat, TrackId) of
      [] ->
         {reply, {errror, no_such_track, [TrackId]}, State};
      [TrackStat] ->
         mnesia:dirty_write(TrackStat#track_stat{subs = Subs}),
         {reply, ok, State}
   end;

on_msg(Msg = {new_track, TrackId, CalcSpeed}, _, State = #state{active_tracks = AT}) ->
   log(State, debug, "~p", [Msg]),
   NewTrackStat = #track_stat{track_id = TrackId, calc_speed = CalcSpeed},
   ok = mnesia:dirty_write(NewTrackStat),
   {reply, {ok, NewTrackStat}, State#state{active_tracks = AT + 1}};

on_msg(Msg = {owner, Pid, TrackId}, _, State) ->
   log(State, debug, "~p", [Msg]),
   case mnesia:dirty_index_read(owner, TrackId, #owner.track_id) of
      [] ->
         log(State, info, "No owner for track ~p found.", [TrackId]);
      [Owner] ->
         log(State, info, "Owner ~p found for track ~p. Will be deleted and unlink.", [Owner, TrackId]),
         mnesia:dirty_delete(owner, Owner#owner.pid),
         unlink(Owner#owner.pid)
   end,
   mnesia:dirty_write(#owner{pid = Pid, track_id = TrackId}),
   link(Pid),
   {reply, ok, State};

on_msg(Msg = {get_track, TrackId}, _, State) ->
   log(State, debug, "~p", [Msg]),
   case mnesia:dirty_read(track_stat, TrackId) of
      [] ->
         {reply, {error, no_such_track, [TrackId]}, State};
      _TrackStat ->
         Res = mnesia:dirty_read(coord, TrackId),
         {reply, Res, State}
   end;

on_msg(Msg = {get_track_stat, TrackId}, _, State) ->
   log(State, debug, "~p", [Msg]),
   case mnesia:dirty_read(track_stat, TrackId) of
      [] ->
         {reply, {error, no_such_track, [TrackId]}, State};
      TrackStat ->
         {reply, TrackStat, State}
   end;

on_msg(Msg = load, _, State = #state{name = Name, active_tracks = AT}) ->
  log(State, debug, "~p", [Msg]),
  {reply, {Name, load, AT}, State};

on_msg(Msg, _From, State) ->
   log(State, error, "Unknown sync message ~p.", [Msg]),
   {noreply, State}.

on_amsg(Coord, State = #state{saver = Saver, coord_received = CR}) when is_record(Coord, coord) ->
   Saver ! Coord,
   {noreply, State#state{coord_received = CR + 1}};

on_amsg(Msg, State) ->
   log(State, error, "Unknown async message ~p.", [Msg]),
   {noreply, State}.

on_info({'EXIT', Pid, _}, State = #state{name = Name, metric_send_period = MetricSendPeriod, saver = Pid}) ->
   log(State, error, "Saver was die. Recreating..."),
   SaverPid = spawn_link(fun() -> saver_loop(saver_name(Name), MetricSendPeriod, now(), 0) end),
   {noreply, State#state{saver = SaverPid}};

on_info({'EXIT', Pid, _}, State) ->
   log(State, info, "Process ~p exited. Trying to find its track.", [Pid]),
   case mnesia:dirty_read(owner, Pid) of
      [#owner{pid = Owner, track_id = TrackId}] ->
         log(State, info, "Track ~p found. Will be closed.", [TrackId]),
         {reply, _, NewState} = on_msg({close, TrackId}, {Pid, undef}, State),
         mnesia:dirty_delete(owner, Owner),
         {noreply, NewState};
      _ ->
         log(State, info, "Track not found for pid ~p.", [Pid]),
         {noreply, State}
   end;

on_info(send_metric, State = #state{name = Name, metric_send_period = MSP, coord_received = CR, active_tracks = AT}) ->
   [{message_queue_len, MQL}, {memory, M}] = process_info(self(), [message_queue_len, memory]),
   CpuUtil = cpu_sup:util(),
   Now = now(),
   send_metric(?metric_collector,
      [
         {Name, Now, ?coord_rate, CR / MSP * 1000},
         {Name, Now, ?message_queue_len, MQL},
         {Name, Now, ?memory, M},
         {net_adm:localhost(), Now, ?cpu, CpuUtil},
         {Name, Now, ?active_tracks, AT}
      ]),
   {noreply, State#state{coord_received = 0}};

on_info(Msg, State) ->
   log(State, error, "Unknown info message ~p.", [Msg]),
   {noreply, State}.

saver_loop(Name, MetricSendPeriod, Timestamp, CoordCount) ->
   F = fun(Coord = #coord{track_id = TrackId}) ->
      [TrackStat] = mnesia:read(track_stat, TrackId),
      UpSubs = gtracker_common:send2subs(TrackStat#track_stat.subs, Coord),
      UpTrackStat = update_track_stat(TrackStat, Coord),
      gtracker_common:send2subs(UpSubs, UpTrackStat),
      mnesia:write(UpTrackStat),
      mnesia:write(Coord)
   end,
   receive
      Coord ->
         {atomic, _} = mnesia:transaction(fun() -> F(Coord) end),
         Now = now(),
         Diff = timer:now_diff(Now, Timestamp),
         if (Diff >= MetricSendPeriod * 1000) ->
            [{message_queue_len, MQL}, {memory, M}] = process_info(self(), [message_queue_len, memory]),
            send_metric(?metric_collector,
               [
                  {Name, Now, ?coord_rate, (CoordCount + 1) / MetricSendPeriod * 1000},
                  {Name, Now, ?message_queue_len, MQL},
                  {Name, Now, ?memory, M}
               ]),
            saver_loop(Name, MetricSendPeriod, now(), 0);
         true ->
            saver_loop(Name, MetricSendPeriod, Timestamp, CoordCount + 1)
         end
   end.


%=======================================================================================================================
%  log helpers
%=======================================================================================================================
log(#state{name = N}, LogLevel, Format, Data) ->
   mds_gen_server:log(N, LogLevel, Format, Data).

log(#state{name = N}, LogLevel, Text) ->
   mds_gen_server:log(N, LogLevel, Text).

%=======================================================================================================================
%  tools
%=======================================================================================================================
-define(create_table(Table, Type),
   case (catch mnesia:table_info(Table, version)) of
      {'EXIT', {aborted, {no_exists, Table, _}}} ->
         mnesia:create_table(
            Table, [{disc_copies, [node()]}, {type, Type}, {attributes, record_info(fields, Table)}]);
      _ ->
         ok
   end).

mnesia_start() ->
   mnesia:create_schema([]),
   mnesia:start(),
   ?create_table(track_stat, ordered_set),
   ?create_table(coord, bag),
   ?create_table(owner, ordered_set),
   mnesia:add_table_index(owner, track_id).

update_track_stat(TrackStat = #track_stat{start = undef}, C = #coord{timestamp = TS}) ->
   update_track_stat(TrackStat#track_stat{start = TS}, C);
update_track_stat(TrackStat = #track_stat{calc_speed = CalcSpeed, avg_speed = AvgSpeed, length = TrackLength, coord_count = Cnt, start = StartTS},
   #coord{speed = Speed, distance = D, timestamp = TS}) ->
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
   TrackStat#track_stat{avg_speed = NewAvgSpeed, coord_count = Cnt + 1, length = TrackLength + D, stop = TS}.

datetime_diff(T2, T1) when (T2 == undef) orelse (T1 == undef) ->
   0;
datetime_diff(T2, T1) ->
   calendar:datetime_to_gregorian_seconds(T2) - calendar:datetime_to_gregorian_seconds(T1).

saver_name({global, Name}) ->
   mds_utils:list_to_atom(atom_to_list(Name) ++ "_saver").

%=======================================================================================================================
%  unit testing facilities
%=======================================================================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
