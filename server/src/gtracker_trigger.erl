-module(gtracker_trigger).

-behaviour(mds_gen_server).

-include("common_defs.hrl").

-export([start/1, stop/0, on_start/1, on_stop/2, on_msg/3, on_amsg/2, on_info/2]).

-import(mds_utils, [get_param/2, get_param/3]).
-import(gtracker_common, [join_pg/2, leave_pg/2, bin_to_urlencoded/1]).
-import(gtracker_format_text, [format_text/8]).

-define(MOD, {local, ?MODULE}).

-record(dev_info, {dev_name, alias, timezone, last_known_pos = undef, triggers = undef, twitter_auth = undef}).
-record(state, {db, gt_pgroup, notif, dev_cache}).

start(Opts) ->
   mds_gen_server:start(?MOD, Opts).

stop() ->
   mds_gen_server:stop(?MOD).

on_start(Opts) ->
   inets:start(),
   SelfOpts = get_param(self, Opts),
   PGroup = get_param(gt_pgroup, SelfOpts, ?DEF_GT_PGROUP),
   DB = get_param(db, SelfOpts),
   Notif = get_param(notif, SelfOpts, undef),
   join_pg(PGroup, self()),
   {ok, #state{db = DB, gt_pgroup = PGroup, notif = Notif, dev_cache = ets:new(dev_cache, [set, {keypos, 2}])}}.

on_stop(Reason, #state{gt_pgroup = PGroup}) ->
   leave_pg(PGroup, self()),
   log(info, "Stopped <~p>.", [Reason]),
   ok.

on_msg(Msg, _From, State) ->
   log(info, "Unknown sync received ~p", [Msg]),
   {reply, ok, State}.

on_amsg(Msg, State) ->
   log(info, "Unknown async message ~p.", [Msg]),
   {noreply, State}.

on_info(?MSG(_From, _GroupName, {coord, first, DevName, {Lat, Lon, _, _, Timestamp} = Coord}),
   #state{db = Db, notif = NotifService, dev_cache = DevCache} = State) ->
   F = fun() ->
         [#dev_info{alias = Alias, timezone = Tz, twitter_auth = TA}] = ets:lookup(DevCache, DevName),
         ets:update_element(DevCache, DevName, {?FieldId(dev_info, last_known_pos), Coord}),
         LocalTs = adjust_timestamp(Timestamp, Tz),
         Triggers = get_triggers(DevName, [online, enter, leave], Db, DevCache),
         process_triggers(NotifService, DevCache, DevName, Alias, Lat, Lon, LocalTs, TA, Triggers)
       end,
   try F()
   catch
      _:Err ->
         log(error, "on_info: online processing failed. Error = ~p", [Err])
   end,
   {noreply, State};

on_info(?MSG(_From, _GroupName, {coord, DevName, {Lat, Lon, _, _, Timestamp} = Coord}),
   #state{db = Db, notif = NotifService, dev_cache = DevCache} = State) ->
   F = fun() ->
         [#dev_info{alias = Alias, timezone = Tz, twitter_auth = TA}] = ets:lookup(DevCache, DevName),
         ets:update_element(DevCache, DevName, {?FieldId(dev_info, last_known_pos), Coord}),
         LocalTs = adjust_timestamp(Timestamp, Tz),
         Triggers = get_triggers(DevName, [enter, leave, periodic], Db, DevCache),
         process_triggers(NotifService, DevCache, DevName, Alias, Lat, Lon, LocalTs, TA, Triggers)
       end,
   try F()
   catch
      _:Err ->
         log(error, "on_info: coord processing failed. Error = ~p", [Err])
   end,
   {noreply, State};

on_info(?MSG(_From, _GroupName, {online, DevName} = Msg), #state{db = Db, dev_cache = DevCache} = State) ->
   log(debug, "~p received", [Msg]),
   {Alias, Timezone, TA} = db_get_device(Db, DevName),
   ets:insert(DevCache, #dev_info{dev_name = DevName, alias = Alias, timezone = Timezone, twitter_auth = TA}),
   {noreply, State};

on_info(?MSG(_From, _GroupName, {MsgType, DevName} = Msg), #state{db = Db, notif = NotifService, dev_cache = DevCache} = State)
   when (MsgType =:= offline) or (MsgType =:= sos) ->
   log(debug, "~p received", [Msg]),
   {Alias, Lat, Lon, Timestamp, TwitterAuth} = case ets:lookup(DevCache, DevName) of
      [] ->
         {undef, undef, undef, undef, undef};
      [#dev_info{alias = A, last_known_pos = undef, twitter_auth = TA}] ->
         {A, undef, undef, undef, TA};
      [#dev_info{alias = A, timezone = Tz, twitter_auth = TA, last_known_pos = {Lt, Ln, _, _, Ts}}] ->
         {A, Lt, Ln, adjust_timestamp(Ts, Tz), TA}
   end,
   try process_triggers(NotifService, DevCache, DevName, Alias, Lat, Lon, Timestamp, TwitterAuth, get_triggers(DevName, [MsgType], Db, DevCache)) of
      _ when (MsgType =:= offline)->
         ets:delete(DevCache, DevName),
         log(info, "~p has been removed from cache", [DevName]);
      _ ->
         ok
   catch
      _:Err ->
         log(error, "on_info: ~p processing failed. Error = ~p", [MsgType, Err])
   end,
   {noreply, State};

on_info(Msg, State) ->
   log(info, "Unknown info received ~p", [Msg]),
   {noreply, State}.

%=======================================================================================================================
%  trigger processing
%=======================================================================================================================
% get_triggers(DevName, Types, Db, DevCache) -> [Trigger]
%     DevName = String()
%     Types = [Type]
%     Type = online | offline | enter | leave | sos
%     Db = database registered process
%     DevCache = ets based devices cache
%     Trigger = trigger(), see include/common_defs.hrl
get_triggers(undef, _, _, _) ->
   [];
get_triggers(DevName, Types, Db, DevCache) ->
   case ets:lookup(DevCache, DevName) of
      [] ->
         [];
      [#dev_info{dev_name = DevName, triggers = []}] ->
         [];
      [#dev_info{dev_name = DevName, triggers = undef}] ->
         case db_get_triggers(Db, DevName) of
            no_triggers ->
               ets:update_element(DevCache, DevName, {?FieldId(dev_info, triggers), []}),
               [];
            Triggers ->
               log(info, "Device ~p triggers are <~p>", [DevName, Triggers]),
               ets:update_element(DevCache, DevName, {?FieldId(dev_info, triggers), Triggers}),
               get_triggers_by_type(Triggers, Types)
            end;
      [#dev_info{dev_name = DevName, triggers = Triggers}] ->
         get_triggers_by_type(Triggers, Types)
   end.

get_triggers_by_type(Triggers, Types) ->
   [T || T <- Triggers, lists:member(erlang:element(?FieldId(trigger, type), T), Types)].

update_trigger(DevCache, DevName, NewTrigger = #trigger{id = Id}) ->
   [DevInfo] = ets:lookup(DevCache, DevName),
   Triggers = DevInfo#dev_info.triggers,
   NewTriggers = lists:keyreplace(Id, ?FieldId(trigger, id), Triggers, NewTrigger),
   ets:update_element(DevCache, DevName, {?FieldId(dev_info, triggers), NewTriggers}).

process_triggers(undef, _DevCache, undef, undef, undef, undef, undef, undef, _) ->
   log(debug, "Nothing to do.");
process_triggers(_NotifService, _DevCache, undef, undef, undef, undef, undef, undef, _) ->
   log(debug, "Nothing to do.");
process_triggers(_NotifService, _DevCache, DevName, _Alias, _Lat, _Lon, _Timestamp, _TA, []) ->
   log(debug, "Nothing to do for device = ~p", [DevName]);
process_triggers(NotifService, DevCache, DevName, Alias, Lat, Lon, Timestamp, TA, [Trigger = #trigger{schedule = Schedule} | Rest]) ->
   case in_service(Timestamp, Schedule) of
      true ->
         process_trigger(NotifService, DevCache, DevName, Alias, Lat, Lon, Timestamp, TA, Trigger),
         process_triggers(NotifService, DevCache, DevName, Alias, Lat, Lon, Timestamp, TA, Rest);
      false ->
         log(debug, "Trigger ~p not scheduled ~p at ~p", [Trigger, Schedule, Timestamp]),
         process_triggers(NotifService, DevCache, DevName, Alias, Lat, Lon, Timestamp, TA, Rest)
   end.

process_trigger(NotifService, _DevCache, _DevName, Alias, Lat, Lon, Timestamp, TwitterAuth,
   Trigger = #trigger{name = TriggerName, type=Type, text = Text})
   when (Type =:= online) or (Type =:= offline) or (Type =:= sos) ->
   Message = format_text(fun log/3, TriggerName, Alias, Type, Lat, Lon, Timestamp, Text),
   send(NotifService, Alias, TwitterAuth, Message, Trigger);

process_trigger(NotifService, DevCache, DevName, Alias, Lat, Lon, Timestamp, TwitterAuth, Trigger = #trigger{name = TriggerName,
      type=periodic = Type, text = Text, config = {period, Period}, executed_at = ExecTime}) ->
   Now = mds_utils:epoch_time(),
   case (ExecTime =:= undef) orelse (Now > ExecTime + Period) of
      true ->
         update_trigger(DevCache, DevName, Trigger#trigger{executed_at = Now}),
         Message = format_text(fun log/3, TriggerName, Alias, Type, Lat, Lon, Timestamp, Text),
         send(NotifService, Alias, TwitterAuth, Message, Trigger);
      false ->
         ok
   end;

process_trigger(NotifService, DevCache, DevName, Alias, Lat, Lon, Timestamp, TwitterAuth,
   Trigger = #trigger{name = TriggerName, type=enter = Type, ready = Ready,
   text = Text, config = {circle, {CheckLat, CheckLon}, Radius}}) ->
   case (nmea_utils:calc_distance({Lat, Lon}, {CheckLat, CheckLon}) =< Radius) of
      true when (Ready =:= true) -> % the trigger shall be triggered
         Message = format_text(fun log/3, TriggerName, Alias, Type, Lat, Lon, Timestamp, Text),
         send(NotifService, Alias, TwitterAuth, Message, Trigger),
         update_trigger(DevCache, DevName, Trigger#trigger{ready = false});
      false when (Ready =:= false) -> % leaving point, but not ready to send, will be reset to ready = true
         update_trigger(DevCache, DevName, Trigger#trigger{ready = true});
      _ ->
         ok
   end;

process_trigger(NotifService, DevCache, DevName, Alias, Lat, Lon, Timestamp, TwitterAuth,
   Trigger = #trigger{name = TriggerName, type=leave = Type, ready = Ready,
   text = Text, config = {circle, {CheckLat, CheckLon}, Radius}}) ->
   case (nmea_utils:calc_distance({Lat, Lon}, {CheckLat, CheckLon}) =< Radius) of
      false when (Ready =:= true) -> % the trigger shall be triggered
         Message = format_text(fun log/3, TriggerName, Alias, Type, Lat, Lon, Timestamp, Text),
         send(NotifService, Alias, TwitterAuth, Message, Trigger),
         update_trigger(DevCache, DevName, Trigger#trigger{ready = false});
      true when (Ready =:= false) -> % we are near point, the trigger became ready
         update_trigger(DevCache, DevName, Trigger#trigger{ready = true});
      _ ->
         ok
   end;

process_trigger(_NotifService, _DevCache, _DevName, _Alias, _Lat, _Lon, _Timestamp, _TA, Trigger) ->
   log(warning, "Unknown trigger ~p.", [Trigger]).

send(NotifService, DevName, TwitterAuth, Message, #trigger{email = EMail, sms = Sms, twitter = UseTwitter}) ->
   send_email(NotifService, EMail, DevName, Message),
   send_sms(NotifService, Sms, DevName, Message),
   send_twit(NotifService, UseTwitter, TwitterAuth, DevName, Message).

send_email(#email{enabled = false}, _, _, _) ->
   ok;
send_email(_, undef, _, _) ->
   ok;
send_email(NotifService, To, Subj, Body) ->
   mds_gen_server:cast(NotifService, {eMail, To, Subj, Body}).

send_sms(#sms{enabled = false}, _, _, _) ->
   ok;
send_sms(_, undef, _, _) ->
   ok;
send_sms(NotifService, PhoneList, DevName, Text) ->
   mds_gen_server:cast(NotifService, {sms, PhoneList, DevName, Text}).

send_twit(undef, _, _, _, _) ->
   ok;
send_twit(_, false, _, _, _) ->
   ok;
send_twit(NotifService, true, TA = {{consumer, _Key, _Secret}, {access, _Token, _TokenSecret}}, DevName, BinText) ->
   mds_gen_server:cast(NotifService, {twitter, TA, DevName, BinText}).

%=======================================================================================================================
%  DB iteraction
%=======================================================================================================================
db_get_triggers(Db, DevName) ->
   mds_gen_server:call(Db, {get_triggers, DevName}).

db_get_device(Db, DevName) ->
   Res = mds_gen_server:call(Db, {get_device, DevName}),
   case Res of
      #device{name = Name, alias = undef, timezone = Tz, twitter_auth = TA} ->
         {Name, Tz, TA};
      #device{alias = Alias, timezone = Tz, twitter_auth = TA} ->
         {Alias, Tz, TA}
   end.

%=======================================================================================================================
%  utils
%=======================================================================================================================
adjust_timestamp(Timestamp, undef) ->
   Timestamp;
adjust_timestamp(Timestamp, Timezone) ->
   localtime:utc_to_local(Timestamp, Timezone).

in_service(_Timestamp, always) ->
   true;
in_service({Date, Time}, Schedule) ->
   case lists:keyfind(calendar:day_of_the_week(Date), 1, Schedule) of
      false ->
         false;
      {_, Periods} ->
         Seconds = calendar:time_to_seconds(Time),
         in_period(Seconds, Periods)
   end.

in_period(_Seconds, []) ->
   false;
in_period(Seconds, [{From, Till} | Rest]) ->
   case (Seconds >= From) and (Seconds =< Till) of
      true ->
         true;
      false ->
         in_period(Seconds, Rest)
   end.

%=======================================================================================================================
%  log helpers
%=======================================================================================================================
log(LogLevel, Format, Data) ->
   mds_gen_server:log(?MODULE, LogLevel, Format, Data).

log(LogLevel, Text) ->
   mds_gen_server:log(?MODULE, LogLevel, Text).

%=======================================================================================================================
%  unit testing facilities
%=======================================================================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

update_trigger_test() ->
   DevName = "DMITRYME_MOB",
   DevCache = ets:new(dev_cache, [set, {keypos, 2}]),
   Trigger1 = #trigger{id = 1, type = sos, name = "sos"},
   Trigger2 = #trigger{id = 2, type = enter, name = "enter"},
   ets:insert(DevCache, #dev_info{dev_name = DevName, alias = DevName, timezone = 600, triggers = [Trigger1, Trigger2]}),
   NewTrigger = #trigger{id = 2, ready = false, type = enter, name = "enter"},
   update_trigger(DevCache, DevName, NewTrigger),
   [DevInfo] = ets:lookup(DevCache, DevName),
   ?assertEqual([Trigger1, NewTrigger], DevInfo#dev_info.triggers).

in_service_test() ->
   Timestamp = {{2010,6,22},{19,11,56}},
   ?assertEqual(true, in_service(Timestamp, always)),
   Schedule = [{2, [{54000, 72000}]}],  % {15:00, 20:00}
   ?assertEqual(true, in_service(Timestamp, Schedule)),
   Schedule1 = [{2, [{54000, 68400}]}],
   ?assertEqual(false, in_service(Timestamp, Schedule1)),
   Schedule2 = [{3, [{54000, 68400}]}],
   ?assertEqual(false, in_service(Timestamp, Schedule2)).

-endif.
