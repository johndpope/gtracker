-module(gtracker_mnesia).

-behaviour(mds_gen_server).

-export([start/1, stop/0, on_start/1, on_stop/2, on_msg/3, on_amsg/2, on_info/2]).

-import(mds_utils, [get_param/2, get_param/3]).
-import(gtracker_common, [gen_dev_name/0, binary_to_hex/1, get_best_process/1]).

-include("common_defs.hrl").
-include("common_recs.hrl").

-define(mod, {global, gtracker_db}).
-define(def_triggers, {global, gtracker_triggers}).
-define(def_track_nodes, gt_tracks).
-record(state, {triggers = undef, track_path = undef, as_track_node = false}).

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
   Triggers = get_param(triggers, SelfOpts, ?def_triggers),
   AsTrackNode = get_param(as_track_node, SelfOpts, false),
   TrackPath = get_param(track_pach, SelfOpts, "/tmp"),
%   crypto:start(),
   mnesia_start(),
   log(info, "Mnesia started."),
   {ok, #state{triggers = Triggers, track_path = TrackPath, as_track_node = AsTrackNode}}.

on_stop(Reason, _State) ->
%   mnesia:stop(),
%   crypto:stop(),
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
            Device = #device{name = DevName, alias = DevName, reference = Ref, online = true, links = #links{owner = Pid}},
            mnesia:dirty_write(Device),
            {reply, Device, State};
         [#device{name = DevName}] ->
            Fun(Fun)
      end
   end,
   Fun(Fun);

on_msg({register, DevName}, {Pid, _}, State = #state{triggers = Triggers}) ->
   log(debug, "register(~p). State: ~p", [DevName, dump_state(State)]),
   case mnesia:dirty_read(device, DevName) of
      [] ->
         {reply, no_such_device, State};
      [Device = #device{links = #links{owner = undef}, online = false}] ->
         NewDevice = activate_device(Device, Pid, Triggers),
         mnesia:dirty_write(NewDevice),
         {reply, NewDevice, State};
      [Device = #device{links = #links{owner = Owner}}] when is_pid(Owner) andalso (Pid == Owner) ->
         {reply, Device, State};
      [Device = #device{links = #links{owner = Owner}}] ->
         case rpc:call(node(Owner), erlang, is_process_alive, [Owner]) of
            true ->
               {reply, Device, State};
            False ->
               log(debug, "is_process_alive(~p): ~p", [Owner, False]),
               NewDevice = activate_device(Device, Pid, Triggers),
               mnesia:dirty_write(NewDevice),
               {reply, NewDevice, State}
         end
   end;

on_msg({unregister, DevName}, {Pid, _}, State) ->
   log(debug, "unregister(~p). State: ~p", [DevName, dump_state(State)]),
   case mnesia:dirty_read(device, DevName) of
      [] ->
         {reply, no_such_device, State};
      [Device = #device{links = #links{owner = undef}, online = false}] ->
         {reply, Device, State};
      [Device = #device{links = #links{owner = Owner}}] when is_pid(Owner) andalso (Pid == Owner) ->
         NewDevice = Device#device{links = #links{}, online = false},
         mnesia:dirty_write(NewDevice),
         {reply, NewDevice, State};
      [Device = #device{links = #links{owner = Owner}}] ->
         case rpc:call(erlang, is_process_alive, [node(Owner), Owner]) of
            true ->
               {reply, wrong_owner, State};
            False ->
               log(debug, "is_process_alive(~p): ~p", [Owner, False]),
               NewDevice = Device#device{links = #links{}, online = false},
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
         {reply, already_exists, State}
   end;

on_msg({get_user, UserName}, _From, State) ->
   log(debug, "get_user(~p). State: ~p", [UserName, dump_state(State)]),
   case mnesia:dirty_read(user, UserName) of
      [] ->
         {reply, not_found, State};
      [User] ->
         {reply, User, State}
   end;

on_msg({update_user, User = #user{name = UserName}}, _From, State) ->
   log(debug, "update_user(~p). State: ~p", [User, dump_state(State)]),
   case mnesia:dirty_read(user, UserName) of
      [] ->
         {reply, no_such_user, State};
      [_] ->
         mnesia:dirty_write(User),
         {reply, User, State}
   end;

on_msg({login, UserName, Password}, _From, State) ->
   log(debug, "login(~p, ~p). State: ~p", [UserName, Password, dump_state(State)]),
   case mnesia:dirty_read(user, UserName) of
      [] ->
         {reply, rejected, State};
      [User = #user{name = U, password = P}] ->
         case (U == UserName) andalso (P == erlang:md5(Password)) of
            true ->
               OnlineUser = User#user{online = true},
               mnesia:dirty_write(OnlineUser),
               {reply, OnlineUser, State};
            false ->
               {reply, rejected, State}
         end
   end;

on_msg({logout, UserName}, _From, State) ->
   log(debug, "logout(~p). State: ~p", [UserName, dump_state(State)]),
   case mnesia:dirty_read(user, UserName) of
      [] ->
         {reply, rejected, State};
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
         {reply, no_such_device, State};
      [Device] ->
         {reply, Device, State}
   end;

on_msg({update_device,
      D = #device{name = DevName, alias = A, links = L, timezone = T, color = C, weight = W, pixmap = P, twitter_auth =
         TA, current_track = CT}}, _From, State) ->
   log(debug, "update_device(~p). State: ~p", [D, dump_state(State)]),
   case mnesia:dirty_read(device, DevName) of
      [] ->
         {reply, no_such_device, State};
      [Device] ->
         NewDevice = Device#device{alias = A, links = L, timezone = T, color = C, weight = W, pixmap = P, twitter_auth =
            TA, current_track = CT},
         mnesia:dirty_write(NewDevice),
         {reply, NewDevice, State}
   end;

on_msg({get_tracks, DevName}, _From, State) ->
   log(debug, "get_tracks(~p). State: ~p", [DevName, dump_state(State)]),
   Tracks = mnesia:dirty_select(track, [{#track{dev_name = '$1', _='_'}, [{'==', '$1', DevName}], ['$_']}]),
   {reply, Tracks, State};

on_msg({get_triggers, DevName}, _From, State) ->
   log(debug, "get_triggers(~p). State: ~p", [DevName, dump_state(State)]),
   Triggers = mnesia:dirty_read(triggers, DevName),
   {reply, Triggers, State};

on_msg({get_news, UpToDate}, _From, State) ->
   log(debug, "get_news(~p). State: ~p", [UpToDate, dump_state(State)]),
   Date = if (UpToDate == undef) -> {2100, 1, 1}; true -> UpToDate end,
   case calendar:valid_date(Date) of
      true ->
         Result = mnesia:dirty_select(news, [{#news{date = '$1', _='_'}, [{'=<', '$1', {Date}}], ['$_']}]),
         {reply, Result, State};
      false ->
         {reply, invalid_date, State}
   end;

on_msg({insert_news, Date, Text}, _From, State) ->
   log(debug, "insert_news(~p, ~p). State: ~p", [Date, Text, dump_state(State)]),
   case valid_date(Date) of
      true ->
         Ref = erlang:make_ref(),
         mnesia:dirty_write(#news{id = Ref, date = Date, text = Text}),
         {reply, Ref, State};
      false ->
         {reply, invalid_date, State}
   end;

on_msg({delete_news, NewsRef}, _From, State) ->
   log(debug, "delete_news(~p). State: ~p", [NewsRef, dump_state(State)]),
   mnesia:dirty_delete(news, NewsRef),
   {reply, ok, State};

on_msg({new_track, DevName, Force, FailuredNodes}, _From, State) ->
   log(debug, "new_track(~p, ~p). State: ~p", [Force, DevName, dump_state(State)]),
   case mnesia:dirty_read(device, DevName) of
      [] ->
         {reply, no_such_device, State};
      [Device = #device{links = #links{track = undef}}] ->
         NewTrack = create_track(Device, Force, FailuredNodes, State), % create new track here
         {reply, NewTrack, State};
      [Device = #device{links = #links{track = TrackPid}}] ->
         case rpc:call(node(TrackPid), erlang, is_process_alive, [TrackPid]) of
            true when (Force == true) ->
               NewTrack = create_track(Device, Force, FailuredNodes, State), % create new track here
               {reply, NewTrack, State};
            true when (Force == false) ->
               {reply, {already_has_active_track, TrackPid}, State};
            false ->
               NewTrack = create_track(Device, Force, FailuredNodes, State), % create new track here
               {reply, NewTrack, State}
         end
   end;

on_msg(Msg, _From, State) ->
   log(error, "Unknown sync message ~p.", [Msg]),
   {noreply, State}.

on_amsg(T = #track_closed{track_id = TrackId, start = Start, stop = Stop, length = Length, avg_speed = AvgSpeed}, State) ->
   log(debug, "track_closed(~p)", [T]),
   F = fun() ->
         case mnesia:read(track, TrackId) of
            [] ->
               log(warning, "track_closed event has been received, but track ~p not exists in DB.", [TrackId]);
            [Track = #track{dev_name = DevName}] ->
               mnesia:write(
                  Track#track{status = closed, start = Start, stop = Stop, length = Length, avg_speed = AvgSpeed}),
               [Device] = mnesia:read(device, DevName),
               if Device#device.current_track == TrackId ->
                     mnesia:write(Device#device{current_track = undef, links = Device#device.links#links{track = undef}});
               true ->
                  ok
               end
         end
   end,
   mnesia:transaction(F),
   {noreply, State};


on_amsg(Msg, State) ->
   log(error, "Unknown async message ~p.", [Msg]),
   {noreply, State}.

on_info({track_stat, _Stat}, State) ->
   log(error, "Statistic for track has been received"),
   {noreply, State};

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
   ?create_table(trigger),
   ?create_table(user),
   ?create_table(track),
   ?create_table(news).

dump_state(State) ->
   State.

get_trigger_process(DevName, Triggers) ->
   case mnesia:dirty_read(trigger, DevName) of
      [] ->
         undef;
      _ ->
         get_best_process(Triggers)
   end.

activate_device(Device = #device{name = DevName, links = Links}, Owner, Triggers) ->
   Device#device
   {
      links = Links#links{owner = Owner, trigger = get_trigger_process(DevName, Triggers)},
      online = true,
      registered_at = now()
   }.

create_track(#device{name = DevName}, _Force, FailuredNodes,
   #state{track_path = TrackPath, as_track_node = AsTrackNode}) ->
   Node = get_best_node(AsTrackNode, FailuredNodes),
   Count = length(mnesia:dirty_select(track, [{#track{dev_name = '$1', _='_'}, [{'==', '$1', DevName}], ['$_']}])) + 1,
   TrackName = list_to_atom(lists:flatten(io_lib:format("~s_~p", [DevName, Count]))),
   NewTrack = #track{id = TrackName, dev_name = DevName, node = Node, path = filename:join(TrackPath, TrackName)},
   mnesia:dirty_write(NewTrack),
   NewTrack.

get_best_node(AsTrackNode, FailuredNodes) ->
   AllNodes = erlang:nodes(if AsTrackNode == true -> [connected, this]; true -> connected end),
   Nodes = lists:subtract(AllNodes, FailuredNodes),
   ProcNodes = lists:foldl(
      fun(Node, Acc) ->
         [{ rpc:call(Node, erlang, system_info, [process_count]), Node} | Acc]
      end, [], Nodes),
   [ {_, BestNode } | _ ] = lists:sort(fun({A, _}, {B, _}) -> A < B end, ProcNodes),
   BestNode.

valid_date(Date = {Y, M, D}) when is_number(Y) andalso is_number(M) andalso is_number(D) ->
   case calendar:valid_date(Date) of
      true ->
         true;
      false ->
         false
   end;

valid_date(_) ->
   false.

%=======================================================================================================================
%  unit testing facilities
%=======================================================================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_device_test() ->
   ok.
   %Pid = gtracker_edb:start([]),
   %timer:sleep(100),
   %{DevName, _} = gen_server:call(?mod, get_device),
   %Device = gen_server:call(?mod, {get_device, DevName}),
   %?assertEqual(DevName, Device#device.name).

get_best_node_test() ->
   ?assertEqual(node(), get_best_node(true, [])).

new_track_test() ->
   ok.

-endif.
