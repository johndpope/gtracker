-module(gtracker_mysql).

-behaviour(mds_gen_server).

-export([start/1, stop/0, on_start/1, on_stop/2, on_msg/3, on_amsg/2, on_info/2]).

-import(mds_utils, [get_param/2, get_param/3]).
-import(gtracker_common, [gen_dev_name/0, ints_to_float/2, join_pg/2, leave_pg/2, get_best_process/1]).

-include("fields.hrl").
-include("common_defs.hrl").

-define(DEF_MAX_TRACK_INTERVAL, 60).  % one minute
-define(DEF_TGS_GROUP, gtracker_tgs_group).
-define(MOD,     {global, gtracker_db}).

-define(LOG_ERROR(MethodName), log(error, "~s failed: ~p. Msg = ~p, State = ~p, Stack trace = ~p", [MethodName, Err,
         Msg, dump_state(State), erlang:get_stacktrace()])).

-record(dev_info, {
                     devname,            %device name, just for speed up
                     device,             % #device
                     track_id = undef    % trackId
                  }).
-record(state, {
                 max_track_interval,   % max interval between last and new tracks. In seconds
                 dev_cache,             % ets of dev_info's
                 tgs_group             % process group, when all trigger processors are registered
               }).

%=======================================================================================================================
%  public exports
%=======================================================================================================================
start(Opts) ->
   mds_gen_server:start(?MOD, ?MODULE, Opts).

stop() ->
   mds_gen_server:stop(?MOD).

%=======================================================================================================================
%  callbacks
%=======================================================================================================================
on_start(Opts) ->
   SelfOpts = get_param(self, Opts),
   Host = get_param(dbhost, SelfOpts),
   Port = get_param(dbport, SelfOpts),
   User = get_param(dbuser, SelfOpts),
   Password = get_param(dbpasswd, SelfOpts),
   DbName = get_param(dbname, SelfOpts),
   TgsGroup = get_param(tgs_group, SelfOpts, ?DEF_TGS_GROUP),
   MaxTrackInterval = get_param(max_track_interval, SelfOpts, ?DEF_MAX_TRACK_INTERVAL),
   gtracker_mysql_exec:start(Host, Port, User, Password, DbName, fun log_callback/4),
   log(info, "Connected to <~p> database as <~p> user on <~p> host.", [DbName, User, Host]),
   {ok, #state{
         max_track_interval = MaxTrackInterval,
         tgs_group = TgsGroup,
         dev_cache = ets:new(dev_cache, [set, {keypos, 2}])}}.

on_stop(Reason, _State) ->
   log(info, "Stopped <~p>.", [Reason]),
   ok.

%=======================================================================================================================
%  sync messages
%=======================================================================================================================
%stop module
on_msg(stop, _From, State) ->
   {stop, normal, stopped, State};

%get new device callback
on_msg(Msg = new_device, _From, State = #state{dev_cache = DevCache}) ->
   log(debug, "new_device(). State = ~p", [dump_state(State)]),
   F = fun(Fun) ->
         DevName = gen_dev_name(),
         log(debug, "Device generated ~p.", [DevName]),
         case gtracker_mysql_exec:select_device(DevName) of
            no_device ->
               gtracker_mysql_exec:start_tran(),
               gtracker_mysql_exec:insert_device(DevName),
               gtracker_mysql_exec:commit_tran(),
               gtracker_mysql_exec:select_device(DevName);
            _Device ->
               Fun(Fun)
         end
      end,
   try F(F) of
     Device =  #device{name = DevName, id = ID, reference = Ref} ->
         ets:insert(DevCache, #dev_info{name = DevName, id = ID, ref = Ref}),
         {reply, Device, State}
   catch
      _:Err ->
         ?LOG_ERROR("get_device/0"),
         {reply, error, State}
   end;

%check existing device callback
on_msg(Msg = {get_device, DevName}, _From, State = #state{dev_cache = DevCache}) ->
   log(debug, "get_device(~p). State = ~p", [DevName, dump_state(State)]),
   try gtracker_mysql_exec:select_device(DevName) of
      no_device ->
         {reply, no_device, State};
      Device = #device{id = DevId, reference = Ref} ->
         ets:insert(DevCache, #dev_info{name = DevName, id = DevId, ref = Ref}),
         {reply, Device, State}
   catch
      _:Err ->
         ?LOG_ERROR("get_device/1"),
         {reply, error, State}
   end;

% select all devices
on_msg(Msg = {get_all_devices, OnlyOnline}, _From, State) ->
   log(debug, "get_all_devices(~p). State = ~p", [OnlyOnline, dump_state(State)]),
   try gtracker_mysql_exec:select_all_devices(OnlyOnline) of
      Result ->
         {reply, Result, State}
   catch
      _:Err ->
         ?LOG_ERROR("get_all_devices"),
         {reply, error, State}
   end;

% get triggers for device
on_msg(Msg = {get_triggers, DevName}, _From, State) ->
   log(debug, "get_triggers(~p). State = ~p", [DevName, dump_state(State)]),
   try gtracker_mysql_exec:select_triggers(DevName) of
      no_triggers ->
         {reply, no_triggers, State};
      Triggers ->
         {reply, Triggers, State}
   catch
      _:Err ->
         ?LOG_ERROR("get_triggers"),
         {reply, error, State}
   end;

on_msg(Msg = {select_tracks, DevName}, _From, State) ->
   log(debug, "select_tracks(~p)", [DevName]),
   try gtracker_mysql_exec:select_tracks(DevName) of
      no_tracks ->
         {reply, no_tracks, State};
      Tracks ->
         {reply, Tracks, State}
   catch
      _:Err ->
         ?LOG_ERROR("select_tracks"),
         {reply, error, State}
   end;

on_msg(Msg = {new_track, DevName, TrackName}, _From, #state{dev_cache = DevCache} = State) ->
   log(debug, "new_track(~p, ~p). State = ~p", [DevName, TrackName, dump_state(State)]),
   F =
   fun() ->
      gtracker_mysql_exec:start_tran(),
      {DevId, _} = get_dev_track(DevName, DevCache),
      stop_track(DevName, DevCache),
      TrackId = gtracker_mysql_exec:new_track(DevId, TrackName),
      gtracker_mysql_exec:commit_tran(),
      gtracker_mysql_exec:select_track(TrackId)
   end,
   try F() of
      Track = #track{id = Id} ->
         set_track_id(DevCache, DevName, Id),
         {reply, {Track, ?MOD}, State}
   catch
      _:Err ->
         ?LOG_ERROR("new_track"),
         {reply, error, State}
   end;

on_msg(Msg = {rename_track, DevName, TrackId, TrackName}, _From, #state{dev_cache = DevCache} = State) ->
   log(debug, "rename_track(~p, ~p). State = ~p", [DevName, TrackName, dump_state(State)]),
   F =
   fun() ->
      gtracker_mysql_exec:start_tran(),
      {_DevId, TrackId} = get_dev_track(DevName, DevCache),
      gtracker_mysql_exec:rename_track(TrackId, TrackName),
      gtracker_mysql_exec:commit_tran(),
      gtracker_mysql_exec:select_track(TrackId),
      {reply, ok, State}
   end,
   try F()
   catch
      _:Err ->
         ?LOG_ERROR("rename_track"),
         {reply, error, State}
   end;

%check existing device callback
on_msg(Msg = {get_device, DevName}, _From, State = #state{dev_cache = DevCache}) ->
   log(debug, "get_device(~p). State = ~p", [DevName, dump_state(State)]),
   try gtracker_mysql_exec:select_device(DevName) of
      no_device ->
         {reply, no_device, State};
      Device = #device{id = DevId, reference = Ref} ->
         ets:insert(DevCache, #dev_info{name = DevName, id = DevId, ref = Ref}),
         {reply, Device, State}
   catch
      _:Err ->
         ?LOG_ERROR("get_device/1"),
         {reply, error, State}
   end;

%register device
on_msg(Msg = {register, DevName}, From, State = #state{tgs_group = TgsGroup, dev_cache = DevCache}) ->
   log(debug, "register(~p, ~p). State: ~p", [DevName, From, dump_state(State)]),
   F = fun() ->
         case gtracker_mysql_exec:select_device(DevName) of
            no_device ->
               {reply, no_device, State};
            Device when (Device#device.registered_by == undef) ->
               gtracker_mysql_exec:set_online(DevName, Owner),
               ets:insert(DevCache, #dev_info{name = DevName, id = Device#device.id, pid = From, ref = Device#device.referece}),
               {reply, {registered, get_best_process(TgsGroup)}, State};
            Owner ->
               {reply, {registered, get_best_process(TgsGroup)}, State};
            RegOwner ->
               log(debug, "OWNER: ~p", [RegOwner]),
               case is_owner_alive(RegOwner) of
                  true ->
                     {reply, already_registered, State};
                  false ->
                     gtracker_mysql_exec:set_online(DevName, Owner),
                     {reply, {registered, get_best_process(TgsGroup)}, State}
               end
         end
   end,
   try F()
   catch
     _:Err ->
        ?LOG_ERROR("registered"),
        {reply, error, State}
   end;

%unregister device
on_msg(Msg = {unregister, DevName}, _From, #state{dev_cache = DevCache} = State) ->
   log(debug, "unregister(~p). State: ~p", [DevName, dump_state(State)]),
   F = fun() ->
         gtracker_mysql_exec:start_tran(),
         gtracker_mysql_exec:set_offline(DevName),
         stop_track(DevName, DevCache),
         gtracker_mysql_exec:commit_tran(),
         set_track_id(DevCache, DevName, undef),
         {reply, unregister, State}
   end,
   try F()
   catch
      _:Err ->
         ?LOG_ERROR("unregister"),
         {reply, error, State}
   end;

% terminator
on_msg(_Msg, _From, State) ->
   {reply, unknown_msg, State}.

%=======================================================================================================================
%  async messages
%=======================================================================================================================
% store first coordiante
on_amsg({coord, first, DevName, NewCoord}, State) ->
   on_amsg({coord, DevName, NewCoord}, State);

on_amsg(Msg = {coord, DevName, NewCoord = {_, _, _, Distance, Timestamp}}, State = #state{dev_cache = DevCache}) ->
   log(debug, "coord. DevName: ~p, NewCoord: ~p, State: ~p", [DevName, NewCoord, dump_state(State)]),
   DevId = get_device_id(DevCache, DevName),
   F = fun() ->
      gtracker_mysql_exec:start_tran(),
      TrackId = get_track(DevName, Timestamp, State),
      gtracker_mysql_exec:insert_coord(TrackId, DevId, NewCoord),
      gtracker_mysql_exec:update_track(TrackId, Distance),
      gtracker_mysql_exec:commit_tran(),
      TrackId
   end,
   try F() of
      TrackId ->
         set_track_id(DevCache, DevName, TrackId),
         {noreply, State}
   catch
      _:Err ->
         ?LOG_ERROR("coord"),
         {noreply, State}
   end;

on_amsg(Msg, State) ->
   log(error, "Unknown async message ~p.", [Msg]),
   {noreply, State}.

%change device
%on_info(?MSG(_From, _GroupName, {change_dev_name, OldDevName, NewDevName}), #state{dev_cache = DevCache} = State) ->
%   F = fun() ->
%         gtracker_mysql_exec:start_tran(),
%         gtracker_mysql_exec:set_offline(OldDevName, DevCache),
%         stop_track(OldDevName, DevCache),
%         case gtraker_mysql:set_online(NewDevName) of
%            true ->
%               gtracker_mysql_exec:commit_tran(),
%               changed;
%            _ ->
%               gtracker_mysql_exec:rollback_tran(),
%               already_online
%         end
%   end,
%   try F()
%   catch
%      _:Err ->
%         log(error, "change_dev_name failed: ~p", [Err])
%   end,
%   {noreply, State};

%terminator
on_info(Msg, State) ->
   log(error, "Unknown info message ~p.", [Msg]),
   {noreply, State}.

%=======================================================================================================================
%  log helpers
%=======================================================================================================================
log_callback(Module, Line, LogLevel, Fun) ->
   {Format, Params} = Fun(),
   Text = lists:flatten(io_lib:format(Format, Params)),
   log(LogLevel, "~p(~p) ~p.", [Module, Line, Text]).

log(LogLevel, Format, Data) ->
   mds_gen_server:log(?MODULE, LogLevel, Format, Data).

log(LogLevel, Text) ->
   mds_gen_server:log(?MODULE, LogLevel, Text).

dump_state({state, _, _, DevCache}) ->
   ets:tab2list(DevCache).

%=======================================================================================================================
%  track manipulations
%=======================================================================================================================
get_dev_track(DevName, DevCache) ->
   Res = ets:lookup(DevCache, DevName),
   case Res of
      [{dev_info, DevName, DevId, _Ref, TrackId}] ->
         {DevId, TrackId};
      _Other ->
         undef
   end.

get_track(DevName, Timestamp, #state{max_track_interval = MaxTrackInterval, dev_cache = DevCache}) ->
   case get_dev_track(DevName, DevCache) of
      {DevId, undef} ->
         start_track(DevId, MaxTrackInterval, Timestamp);
      {_DevId, TrackId} ->
         TrackId;
      undef ->
         throw(no_device_found)
   end.

start_track(DevId, MaxTrackInterval, Timestamp) ->
   case gtracker_mysql_exec:select_last_track(DevId) of
     no_track -> % where are no tracks for such device
        log(info, "Device with id = ~p doesn't have tracks. Inserting new one...", [DevId]),
        gtracker_mysql_exec:new_track(DevId);
     {TrackId, undef} -> % empty track
        gtracker_mysql_exec:reopen_track(TrackId),
        log(info, "Device with id = ~p has an empty track ~p. Will be reused.", [DevId, TrackId]),
        TrackId;
     {TrackId, LastCoordTm} ->
       log(debug, "Old track with Id = ~p and LastCoordTm = ~p selected.", [TrackId, LastCoordTm]),
       case calendar:datetime_to_gregorian_seconds(LastCoordTm) + MaxTrackInterval
          >= calendar:datetime_to_gregorian_seconds(Timestamp) of
          true -> % old track is to be restarted
             gtracker_mysql_exec:reopen_track(TrackId),
             log(debug, "Old track will be reused."),
             TrackId;
          false -> % new track will be started
             log(debug, "New track will be started."),
             gtracker_mysql_exec:new_track(DevId)
       end
   end.

stop_track(DevName, DevCache) ->
   case ets:lookup(DevCache, DevName) of
      [] ->
         no_track;
      [{dev_info, DevName, _, _, undef}] ->
         no_track;
      [{dev_info, DevName, _, _, TrackId}] ->
         gtracker_mysql_exec:stop_track(TrackId),
         TrackId
   end.

get_device_id(DevCache, DevName) ->
   case ets:lookup(DevCache, DevName) of
      [{dev_info, DevName, DevId, _, _}] ->
         DevId;
      [] ->
         throw(device_not_found)
   end.

set_track_id(DevCache, DevName, TrackId) ->
   ets:update_element(DevCache, DevName, {?FieldId(dev_info, track_id), TrackId}).

is_owner_alive({Pid, Node}) ->
   case (catch rpc:call(Node, erlang, is_process_alive, [Pid])) of
      {badrpc, nodedown} ->
         false;
      {badrpc, Reason} ->
         throw(Reason);
      Res ->
         Res
   end.
