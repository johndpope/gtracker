-module(gtracker_mnesia).

-behaviour(mds_gen_server).

-export([start/1, stop/0, on_start/1, on_stop/2, on_msg/3, on_amsg/2, on_info/2]).

-import(mds_utils, [get_param/2, get_param/3]).
-import(gtracker_common, [gen_dev_name/0, binary_to_hex/1, get_best_node/3]).

-include("common_defs.hrl").
-include("common_recs.hrl").

-define(mod, {global, gtracker_db}).
-define(def_track_nodes, gt_tracks).
-record(state, {track_ns, track_path = undef, as_track_node = false}).

%=======================================================================================================================
%  public exports
%=======================================================================================================================
start(Opts) ->
   mds_gen_server:start(?mod, ?MODULE, Opts).

stop() ->
   mds_gen_server:stop(?mod).

%=======================================================================================================================
%  callbacks
%=======================================================================================================================
on_start(Opts) ->
   SelfOpts = get_param(self, Opts),
   AsTrackNode = get_param(as_track_node, SelfOpts, false),
   TrackNS = get_param(track_ns, SelfOpts, undef),
   TrackPath = get_param(track_path, SelfOpts, "/tmp"),
   mnesia_start(),
   process_flag(trap_exit, true),
   log(info, "Mnesia started."),
   {ok, #state{track_ns = TrackNS, track_path = TrackPath, as_track_node = AsTrackNode}}.

on_stop(Reason, _State) ->
   log(info, "Mnesia stopped."),
   log(info, "Stopped <~p>.", [Reason]),
   ok.

on_msg(stop, _From, State) ->
   {stop, normal, stopped, State};

on_msg(register, {Pid, _}, State) ->
   log(debug, "register. State: ~p", [dump_state(State)]),
   Fun = fun(Fun) ->
      DevName = gen_dev_name(),
      log(debug, "Device generated ~p.", [DevName]),
      case mnesia:dirty_read(device, DevName) of
         [] ->
            log(debug, "Store device ~p", [DevName]),
            Ref = binary_to_hex(erlang:md5(erlang:list_to_binary(DevName))),
            Device = #device{name = DevName, alias = DevName, reference = Ref, status = online, owner = Pid},
            erlang:link(Pid),
            mnesia:dirty_write(Device),
            {reply, Device, State};
         [#device{name = DevName}] ->
            Fun(Fun)
      end
   end,
   Fun(Fun);

on_msg({register, DevName}, {Pid, _}, State) ->
   log(debug, "register(~p). State: ~p", [DevName, dump_state(State)]),
   case mnesia:dirty_read(device, DevName) of
      [] ->
         {reply, {error, no_such_device, [DevName]}, State};
      [Device = #device{owner = undef, status = offline}] ->
         NewDevice = activate_device(Device, Pid),
         mnesia:dirty_write(NewDevice),
         {reply, NewDevice, State};
      [Device = #device{owner = Owner}] when is_pid(Owner) andalso (Pid == Owner) ->
         {reply, Device, State};
      [Device = #device{owner = Owner, current_track = TrackId}] ->
         log(info, "Device ~p", [Device]),
         case mnesia:dirty_read(track, TrackId) of
            [] ->
               ok;
            [Track] ->
               log(info, "Track ~p found. Set new owner ~p to him.", [Track, Pid]),
               gtracker_track_pub:set_owner(Track, Pid)
         end,
         log(info, "Trying to stop old owner with Pid = ~p", [Owner]),
         Owner ! stop,
         NewDevice = activate_device(Device, Pid),
         mnesia:dirty_write(NewDevice),
         {reply, NewDevice, State}
   end;

on_msg({unregister, DevName}, {Pid, _}, State) ->
   log(debug, "unregister(~p). State: ~p", [DevName, dump_state(State)]),
   case mnesia:dirty_read(device, DevName) of
      [] ->
         {reply, {error, no_such_device, [DevName]}, State};
      [Device = #device{owner = undef, status = offline, subs = Subs}] ->
         NewSubs = gtracker_common:send2subs(Subs, {offline, DevName}),
         if (NewSubs =/= Subs) ->
            mnesia:dirty_write(Device#device{subs = NewSubs}),
            {reply, Device, State};
         true ->
            {reply, Device, State}
         end;
      [Device = #device{owner = Owner, subs = Subs}] when is_pid(Owner) andalso (Pid == Owner) ->
         NewSubs = gtracker_common:send2subs(Subs, {offline, DevName}),
         NewDevice = Device#device{owner = undef, status = offline, subs = NewSubs},
         mnesia:dirty_write(NewDevice),
         {reply, NewDevice, State};
      [Device = #device{owner = Owner, subs = Subs}] ->
         case rpc:call(node(Owner), erlang, is_process_alive, [Owner]) of
            true ->
               {reply, {error, wrong_owner, [Pid, Owner]}, State};
            False ->
               log(debug, "is_process_alive(~p): ~p", [Owner, False]),
               NewSubs = gtracker_common:send2subs(Subs, {offline, DevName}),
               NewDevice = Device#device{owner = undef, status = offline, subs = NewSubs},
               mnesia:dirty_write(NewDevice),
               {reply, NewDevice, State}
         end
   end;

on_msg({new_user, UserName, Password}, _From, State) ->
   log(debug, "new_user(~p, ~p). State: ~p", [UserName, Password, dump_state(State)]),
   case mnesia:dirty_read(user, UserName) of
      [] ->
         User = #user{name = UserName, password = erlang:md5(Password)},
         mnesia:dirty_write(User),
         {reply, User, State};
      _User ->
         {reply, {error, already_exists, [UserName]}, State}
   end;

on_msg({get_user, UserName}, _From, State) ->
   log(debug, "get_user(~p). State: ~p", [UserName, dump_state(State)]),
   case mnesia:dirty_read(user, UserName) of
      [] ->
         {reply, {error, not_found, [UserName]}, State};
      [User] ->
         {reply, User, State}
   end;

on_msg({update, NewUser = #user{name = UserName}, Mask}, _From, State) ->
   log(debug, "update(~p, ~p). State: ~p", [NewUser, Mask, dump_state(State)]),
   F = fun() ->
      case mnesia:dirty_read(user, UserName) of
         [] ->
            {error, no_such_user, [UserName]};
         [User] ->
            MergedUser = merge_users(User, NewUser, Mask),
            mnesia:dirty_write(MergedUser),
            MergedUser
      end
   end,
   try F() of
      Res ->
         {reply, Res, State}
   catch
      _:Err ->
         {reply, Err, State}
   end;

on_msg({login, UserName, Password}, _From, State) ->
   log(debug, "login(~p, ~p). State: ~p", [UserName, Password, dump_state(State)]),
   case mnesia:dirty_read(user, UserName) of
      [] ->
         {reply, {error, rejected, [UserName, Password]}, State};
      [User = #user{name = U, password = P}] ->
         case (U == UserName) andalso (P == erlang:md5(Password)) of
            true ->
               OnlineUser = User#user{online = true},
               mnesia:dirty_write(OnlineUser),
               {reply, OnlineUser, State};
            false ->
               {reply, {error, rejected, [UserName, Password]}, State}
         end
   end;

on_msg({logout, UserName}, _From, State) ->
   log(debug, "logout(~p). State: ~p", [UserName, dump_state(State)]),
   case mnesia:dirty_read(user, UserName) of
      [] ->
         {reply, {error, rejected, [UserName]}, State};
      [User = #user{name = UserName}] ->
         mnesia:dirty_write(User#user{online = false}),
         {reply, ok, State}
   end;

on_msg(get_all_devices, _From, State) ->
   log(debug, "get_all_devices. State: ~p", [dump_state(State)]),
   Devices = mnesia:dirty_select(device, [{'_', [], ['$_']}]),
   {reply, Devices, State};

on_msg({get_device, DevName}, _From, State) ->
   log(debug, "get_device(~p). State: ~p", [DevName, dump_state(State)]),
   case mnesia:dirty_read(device, DevName) of
      [] ->
         {reply, {error, no_such_device, [DevName]}, State};
      [Device] ->
         {reply, Device, State}
   end;

on_msg({update, NewDevice = #device{name = DevName}, Mask}, _From, State) ->
   log(debug, "update(~p). State: ~p", [NewDevice, dump_state(State)]),
   F = fun() ->
      case mnesia:dirty_read(device, DevName) of
         [] ->
            {error, no_such_device, [DevName]};
         [Device = #device{subs = Subs}] ->
            MergedDevice = merge_devices(Device, NewDevice, Mask),
            NewSubs = gtracker_comon:send2subs(Subs, {updated, MergedDevice}),
            MergedDevice2 = MergedDevice#device{subs = NewSubs},
            mnesia:dirty_write(MergedDevice2),
            MergedDevice2
      end
   end,
   try F() of
      Res ->
         {reply, Res, State}
   catch
      _:Err ->
         {reply, Err, State}
   end;

on_msg({subscribe, DevName, Pid}, _From, State) ->
   log(debug, "subscribe(~p, ~p). State: ~p", [DevName, Pid, dump_state(State)]),
   case mnesia:dirty_read(device, DevName) of
      [] ->
         {reply, {error, no_such_device, [DevName]}, State};
      [Device = #device{status = Status, subs = Subs}] ->
         NewDevice = Device#device{subs = lists:usort([Pid|Subs])},
         Pid ! {Status, Device#device.name},
         mnesia:dirty_write(NewDevice),
         {reply, NewDevice, State}
   end;

on_msg({unsubscribe, DevName, Pid}, _From, State) ->
   log(debug, "unsubscribe(~p, ~p). State: ~p", [DevName, Pid, dump_state(State)]),
   case mnesia:dirty_read(device, DevName) of
      [] ->
         {reply, {error, no_such_device, [DevName]}, State};
      [Device = #device{subs = Subs}] ->
         NewDevice = Device#device{subs = lists:delete(Pid, Subs)},
         mnesia:dirty_write(NewDevice),
         {reply, NewDevice, State}
   end;

on_msg({get_tracks, DevName}, _From, State) ->
   log(debug, "get_tracks(~p). State: ~p", [DevName, dump_state(State)]),
   Tracks = mnesia:dirty_index_read(track, DevName, #track.dev_name),
   {reply, Tracks, State};

on_msg({get_news, UpToDate}, _From, State) ->
   log(debug, "get_news(~p). State: ~p", [UpToDate, dump_state(State)]),
   Date = if (UpToDate == undef) -> {2100, 1, 1}; true -> UpToDate end,
   case calendar:valid_date(Date) of
      true ->
         Result = mnesia:dirty_select(news, [{#news{date = '$1', _='_'}, [{'=<', '$1', {Date}}], ['$_']}]),
         {reply, Result, State};
      false ->
         {reply, {error, invalid_date, [UpToDate]}, State}
   end;

on_msg({insert_news, Date, Text}, _From, State) ->
   log(debug, "insert_news(~p, ~p). State: ~p", [Date, Text, dump_state(State)]),
   case valid_date(Date) of
      true ->
         Ref = erlang:make_ref(),
         mnesia:dirty_write(#news{id = Ref, date = Date, text = Text}),
         {reply, Ref, State};
      false ->
         {reply, {error, invalid_date, [Date]}, State}
   end;

on_msg({delete_news, NewsRef}, _From, State) ->
   log(debug, "delete_news(~p). State: ~p", [NewsRef, dump_state(State)]),
   mnesia:dirty_delete(news, NewsRef),
   {reply, ok, State};

on_msg({new_track, DevName, Force, FailuredNodes}, _From, State) ->
   log(debug, "new_track(~p, ~p). State: ~p", [Force, DevName, dump_state(State)]),
   case mnesia:dirty_read(device, DevName) of
      [] ->
         {reply, {error, no_such_device, {DevName}}, State};
      [#device{owner = undef}] ->
         {reply, {error, device_not_registered, [DevName]}, State};
      [Device = #device{current_track = undef}] ->
         NewTrack = create_track(Device, Force, FailuredNodes, State), % create new track here
         mnesia:dirty_write(Device#device{current_track = NewTrack#track.id}),
         {reply, NewTrack, State};
      [Device = #device{current_track = TrackId}] ->
         case mnesia:dirty_read(track, TrackId) of
            [] ->
               NewTrack = create_track(Device, Force, FailuredNodes, State), % create new track here
               mnesia:dirty_write(Device#device{current_track = NewTrack#track.id}),
               {reply, NewTrack, State};
            [Track = #track{pid = Pid}] ->
               case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
                  true when (Force == true) ->
                     NewTrack = create_track(Device, Force, FailuredNodes, State), % create new track here
                     mnesia:dirty_write(Device#device{current_track = NewTrack#track.id}),
                     {reply, NewTrack, State};
                  true when (Force == false) ->
                     {reply, Track, State};
                  false ->
                     NewTrack = create_track(Device, Force, FailuredNodes, State), % create new track here
                     mnesia:dirty_write(Device#device{current_track = NewTrack#track.id}),
                     {reply, NewTrack, State}
               end
         end
   end;

on_msg({update, NewTrack = #track{id = TrackId}, Mask}, _From, State) ->
   log(debug, "update(~p, ~p). State: ~p", [NewTrack, Mask, dump_state(State)]),
   F = fun() ->
         case mnesia:dirty_read(track, TrackId) of
            [] ->
               {error, no_such_track, [TrackId]};
            [Track = #track{pid = Pid}] ->
               MergedTrack = merge_tracks(Track, NewTrack, Mask),
               mnesia:dirty_write(MergedTrack),
               if is_pid(Pid) ->
                  Pid ! {updated, MergedTrack},
                  MergedTrack;
               true ->
                  MergedTrack
               end
         end
   end,
   try F() of
      Res ->
         {reply, Res, State}
   catch
      _:Err ->
         {reply, Err, State}
   end;

on_msg(Msg, _From, State) ->
   log(error, "Unknown sync message ~p.", [Msg]),
   {noreply, State}.

on_amsg({closed, NewTrack = #track{id = TrackId}}, State) ->
   log(debug, "{closed, ~p}", [NewTrack]),
   F = fun() ->
         case mnesia:read(track, TrackId) of
            [] ->
               log(warning, "track_closed event has been received, but track ~p not exists in DB.", [TrackId]);
            [Track = #track{dev_name = DevName}] ->
               MergedTrack = merge_tracks(Track, NewTrack, [length, avg_speed, start, stop, coord_count]),
               mnesia:write(MergedTrack),
               [Device] = mnesia:read(device, DevName),
               if Device#device.current_track == TrackId ->
                  mnesia:write(Device#device{current_track = undef});
               true ->
                  ok
               end
         end
   end,
   mnesia:transaction(F),
   {noreply, State};

on_amsg({updated, NewTrack = #track{id = TrackId}}, State) ->
   log(debug, "{updated, ~p}", [NewTrack]),
   case mnesia:dirty_read(track, TrackId) of
      [] ->
         log(warning, "track_updated event has been received, but track ~p not exists in DB.", [TrackId]);
      [Track] ->
         MergedTrack = merge_tracks(Track, NewTrack, [length, avg_speed, start, stop, coord_count]),
         mnesia:dirty_write(MergedTrack)
   end,
   {noreply, State};

on_amsg(Msg, State) ->
   log(error, "Unknown async message ~p.", [Msg]),
   {noreply, State}.

on_info({track_stat, _Stat}, State) ->
   log(error, "Statistic for track has been received"),
   {noreply, State};

on_info({'EXIT', Pid, _}, State) ->
   log(info, "Process ~p exited. Trying to update device", [Pid]),
   case mnesia:dirty_select(device, [{#device{owner = '$1', _='_'}, [{'==', '$1', Pid}], ['$_']}]) of
      [Device] ->
         log(info, "Device ~p found. Will be unregistered.", [Device#device.name]),
         {reply, _, NewState} = on_msg({unregister, Device#device.name}, {Pid, undef}, State),
         {noreply, NewState};
      _ ->
         log(info, "Device owned by ~p not found.", [Pid]),
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
-define(create_table(Table),
   case (catch mnesia:table_info(Table, version)) of
      {'EXIT', {aborted, {no_exists, Table, _}}} ->
         mnesia:create_table(
            Table, [{disc_copies, [node()]}, {type, ordered_set}, {attributes, record_info(fields, Table)}]);
      _ ->
         ok
   end).

mnesia_start() ->
   mnesia:create_schema([]),
   mnesia:start(),
   ?create_table(device),
   ?create_table(user),
   ?create_table(track),
   ?create_table(news),
   mnesia:add_table_index(track, dev_name).

dump_state(State) ->
   State.

activate_device(Device = #device{name = DevName, subs = Subs}, Owner) ->
   erlang:link(Owner),
   NewSubs = gtracker_common:send2subs(Subs, {online, DevName}),
   Device#device
   {
      owner = Owner,
      subs = NewSubs,
      status = online,
      registered_at = now()
   }.

create_track(#device{name = DevName}, _Force, FailuredNodes,
   #state{track_ns = TrackNS, track_path = TrackPath, as_track_node = AsTrackNode}) ->
   F = fun(Suffix) ->
         Node = get_best_node(TrackNS, AsTrackNode, FailuredNodes),
         TrackName = list_to_atom(lists:flatten(io_lib:format("~s_~p.track", [DevName, Suffix]))),
         NewTrack = #track{id = TrackName, dev_name = DevName, node = Node, path = filename:join(TrackPath, TrackName)},
         mnesia:dirty_write(NewTrack),
         NewTrack
   end,
   case get_last_track(DevName) of
      [] ->
         F(1);
      T = #track{stop = undef} ->
         T;
      T = #track{id = Id, stop = Timestamp} ->
         Diff = calendar:datetime_to_gregorian_seconds(calendar:universal_time()) -
            calendar:datetime_to_gregorian_seconds(Timestamp),
         if (Diff > 600) -> % start new track
            F(get_track_suffix(Id));
         true -> % use old one
            T
         end
   end.

get_track_suffix(Id) ->
   TrackName = atom_to_list(Id),
   {ok, MP} = re:compile(".*_([0-9]+)"),
   {match, [LSuffix]} = re:run(TrackName, MP, [{capture, [1], list}]),
   list_to_integer(LSuffix).

get_last_track(DevName) ->
   Tracks = mnesia:dirty_select(track, [{#track{dev_name = '$1', _='_'}, [{'==', '$1', DevName}], ['$_']}]),
   case lists:sort(fun(#track{id = A}, #track{id = B}) -> A >= B end, Tracks) of
      [] ->
         [];
      [Track|_] ->
         Track
   end.

valid_date(Date = {Y, M, D}) when is_number(Y) andalso is_number(M) andalso is_number(D) ->
   case calendar:valid_date(Date) of
      true ->
         true;
      false ->
         false
   end;

valid_date(_) ->
   false.

merge_users(#user{name = Username1}, #device{name = Username2}, _) when Username1 =/= Username2 ->
   throw({error, unable_to_merge_diff_users, [Username1, Username2]});
merge_users(User, NewUser, Mask) ->
   check_mask(
      Mask,
      [password, map_type, is_admin, devices],
      fun(ValidatedMask) -> merge_users_aux(User, NewUser, ValidatedMask) end).

merge_devices(#device{name = DevName1}, #device{name = DevName2}, _) when DevName1 =/= DevName2 ->
   throw({error, unable_to_merge_diff_devices, [DevName1, DevName2]});
merge_devices(Device, NewDevice, Mask) ->
   check_mask(
      Mask,
      [alias, owner, subs, timezone, color, weight, pixmap, twitter_auth, current_track],
      fun(ValidatedMask) -> merge_devices_aux(Device, NewDevice, ValidatedMask) end).

merge_tracks(#track{id = ID1}, #track{id = ID2}, _) when ID1 =/= ID2 ->
   throw({error, unable_to_merge_diff_tracks, [ID1, ID2]});
merge_tracks(Track, NewTrack, Mask) ->
   check_mask(
      Mask,
      [name, pid, start, stop, length, avg_speed, coord_count],
      fun(ValidatedMask) -> merge_tracks_aux(Track, NewTrack, ValidatedMask) end).

merge_users_aux(User, _, []) ->
   User;
merge_users_aux(User, NewUser, [Field|Rest]) ->
   Index = ?FieldId(user, Field),
   Value = erlang:element(Index, NewUser),
   merge_users_aux(erlang:setelement(Index, User, Value), NewUser, Rest).

merge_tracks_aux(Track, _, []) ->
   Track;
merge_tracks_aux(Track, NewTrack, [Field|Rest]) ->
   Index = ?FieldId(track, Field),
   Value = erlang:element(Index, NewTrack),
   merge_tracks_aux(erlang:setelement(Index, Track, Value), NewTrack, Rest).

merge_devices_aux(Device, _, []) ->
   Device;
merge_devices_aux(Device, NewDevice, [Field|Rest]) ->
   Index = ?FieldId(device, Field),
   Value = erlang:element(Index, NewDevice),
   merge_devices_aux(erlang:setelement(Index, Device, Value), NewDevice, Rest).

check_mask(Mask, AlowedFields, Fun) ->
   {ValidatedMask, InvalidMask} = lists:partition(fun(E) -> lists:member(E, AlowedFields) end, lists:usort(Mask)),
   case length(InvalidMask) > 0 of
      true ->
         throw({error, invalid_mask_elements, InvalidMask});
      false ->
         Fun(ValidatedMask)
   end.

%=======================================================================================================================
%  unit testing facilities
%=======================================================================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


merge_tracks_test() ->
   Track1 = #track{id='1'},
   Track2 = #track{id='1', name = "TrackName", start={1,2,3}, stop={2,3,4}, length = 1000, avg_speed = 45.0},
   MergedTrack1 = merge_tracks(Track1, Track2, [name, start, stop, length, avg_speed]),
   ?assertEqual(MergedTrack1, Track2),
   MergedTrack2 = merge_tracks(Track1, Track2, [name]),
   ?assertEqual(MergedTrack2, #track{id='1', name="TrackName"}),
   ?assertThrow({error, unable_to_merge_diff_tracks, ['1', '2']}, merge_tracks(Track1, #track{id='2', name="BLA"},[name])),
   ?assertThrow({error, invalid_mask_elements, [id, name1]}, merge_tracks(Track1, #track{id='1', name="BLA"}, [id, name1])).

new_track_test() ->
   ok.

-endif.
