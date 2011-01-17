-module(gtracker_track).

-behaviour(mds_gen_server).

-export([start/1, stop/0, on_start/1, on_stop/2, on_msg/3, on_amsg/2, on_info/2]).

-import(mds_utils, [get_param/2, get_param/3]).
-import(gtracer_common, [join_pg/2, leave_pg/2]).

-include("common_defs.hrl").
-include("common_recs.hrl").

-record(state, {name, calc_speed, pg}).
-record(owner, {pid, track_id}).

%=======================================================================================================================
%  public exports
%=======================================================================================================================
start(Opts) ->
   SelfOpts = get_param(self, Opts),
   ServName = {global, _} = get_param(name, SelfOpts), % should have {global, Atom()} format
   mds_gen_server:start(ServName, ?MODULE, Opts).

stop() ->
   mds_gen_server:stop(?track_ref).

%=======================================================================================================================
%  callbacks
%=======================================================================================================================
on_start(Opts) ->
   SelfOpts = get_param(self, Opts),
   CalcSpeed = get_param(calc_speed, SelfOpts, false),
   ServName = get_param(name, SelfOpts),
   ProcGroup = get_param(group, SelfOpts, track),
   mnesia_start(),
   process_flag(trap_exit, true),
   join_pg(ProcGroup, self()),
   log(info, "Track started."),
   {ok, #state{name = ServName, pg = ProcGroup, calc_speed = CalcSpeed}}.

on_stop(Reason, State) ->
   leave_pg(State#state.pg, self()),
   log(info, "Stopped <~p>.", [Reason]),
   ok.

on_msg(stop, _From, State) ->
   {stop, normal, stopped, State};

on_msg(Msg = name, _, State) ->
   log(debug, "~p", [Msg]),
   {reply, {ok, State#state.name}, State};

on_msg(Msg = process_info, _, State) ->
   log(debug, "~p", [Msg]),
   [{_, Size}] = process_info(self(), [message_queue_len]),
   {reply, Size, State};

on_msg(Msg = {close, TrackId}, _, State) ->
   log(debug, "~p", [Msg]),
   case mnesia:dirty_read(track_stat, TrackId) of
      [] ->
         {reply, {errror, no_such_track, [TrackId]}, State};
      [TrackStat] ->
         UpTrackStat = TrackStat#track_stat{subs = [], status = closed},
         gtracker_common:send2subs(TrackStat#track_stat.subs, UpTrackStat),
         mnesia:dirty_write(UpTrackStat),
         {reply, ok, State}
   end;

on_msg(Msg = {subscribers, TrackId, Subs}, _, State) ->
   log(debug, "~p", [Msg]),
   case mnesia:dirty_read(track_stat, TrackId) of
      [] ->
         {reply, {errror, no_such_track, [TrackId]}, State};
      [TrackStat] ->
         mnesia:dirty_write(TrackStat#track_stat{subs = Subs}),
         {reply, ok, State}
   end;

on_msg(Msg = {owner, Pid, TrackId}, _, State) ->
   log(debug, "~p", [Msg]),
   case mnesia:dirty_index_read(owner, TrackId, #owner.track_id) of
      [] ->
         log(info, "No owner for track ~p found.", [TrackId]);
      [Owner] ->
         log(info, "Owner ~p found for track ~p. Will be deleted and unlink.", [Owner, TrackId]),
         mnesia:dirty_delete(owner, Owner#owner.pid),
         unlink(Owner#owner.pid)
   end,
   mnesia:dirty_write(#owner{pid = Pid, track_id = TrackId}),
   link(Pid),
   {reply, ok, State};

on_msg(Msg = {get_track, TrackId}, _, State) ->
   log(debug, "~p", [Msg]),
   case mnesia:dirty_read(track_stat, TrackId) of
      [] ->
         {reply, {error, no_such_track, [TrackId]}, State};
      _TrackStat ->
         Res = mnesia:dirty_read(coord, TrackId),
         {reply, Res, State}
   end;

on_msg(Msg = {get_track_stat, TrackId}, _, State) ->
   log(debug, "~p", [Msg]),
   case mnesia:dirty_read(track_stat, TrackId) of
      [] ->
         {reply, {error, no_such_track, [TrackId]}, State};
      TrackStat ->
         {reply, TrackStat, State}
   end;

on_msg(Msg, _From, State) ->
   log(error, "Unknown sync message ~p.", [Msg]),
   {noreply, State}.

on_amsg(Coord = #coord{track_id = TrackId}, State = #state{calc_speed = CalcSpeed}) ->
   F = fun() ->
      TrackStat =
      case mnesia:read(track_stat, TrackId) of
         [TS] ->
            TS;
         [] ->
            mnesia:dirty_write(#track_stat{track_id = TrackId})
      end,
      UpSubs = gtracker_common:send2subs(TrackStat#track_stat.subs, Coord),
      UpTrackStat = update_track_stat(TrackStat, Coord, CalcSpeed),
      gtracker_common:send2subs(UpSubs, UpTrackStat),
      mnesia:write(UpTrackStat),
      mnesia:write(Coord),
      UpSubs = gtracker_common:send2subs(TrackStat#track_stat.subs, Coord),
      gtracker_common:send2subs(UpSubs, UpTrackStat)
   end,
   mnesia:transaction(F()),
   {noreply, State};

on_amsg(Msg, State) ->
   log(error, "Unknown async message ~p.", [Msg]),
   {noreply, State}.

on_info({'EXIT', Pid, _}, State) ->
   log(info, "Process ~p exited. Trying to find its track.", [Pid]),
   case mnesia:dirty_read(owner, Pid) of
      [TrackId] ->
         log(info, "Track ~p found. Will be closed.", [TrackId]),
         {reply, _, NewState} = on_msg({close, TrackId}, {Pid, undef}, State),
         {noreply, NewState};
      _ ->
         log(info, "Track not foud for pid ~p.", [Pid]),
         {noreply, State}
   end;

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

update_track_stat(TrackStat = #track_stat{start = undef}, C = #coord{timestamp = TS}, CalcSpeed) ->
   update_track_stat(TrackStat#track_stat{start = TS}, C, CalcSpeed);
update_track_stat(TrackStat = #track_stat{avg_speed = AvgSpeed, length = TrackLength, coord_count = Cnt, start = StartTS},
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
   TrackStat#track_stat{avg_speed = NewAvgSpeed, coord_count = Cnt + 1, length = TrackLength + D, stop = TS}.

datetime_diff(T2, T1) when (T2 == undef) orelse (T1 == undef) ->
   0;
datetime_diff(T2, T1) ->
   calendar:datetime_to_gregorian_seconds(T2) - calendar:datetime_to_gregorian_seconds(T1).

%=======================================================================================================================
%  unit testing facilities
%=======================================================================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
