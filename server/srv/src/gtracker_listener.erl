-module(gtracker_listener).

-author(dmitryme).

-behaviour(mds_gen_server).

-include_lib("stdlib/include/qlc.hrl").
-include("common_defs.hrl").

-export([start/1, stop/0, on_start/1, on_stop/2, on_msg/3, on_amsg/2, on_info/2]).

-export([start_in_shell/0]).

-import(mds_utils, [get_param/2, get_param/3]).
-import(gtracker_common, [send_pg/2]).


-define(TIMEOUT, 100).
-define(PORT, 7777).
-define(MOD, {global, ?MODULE}).

-record(state, {lsocket, gt_pgroup, db, devices, opts}).

start_in_shell() ->
   start([{root_dir, "/tmp/gtracker"}, {db, nodb}, {log_level, debug}]).

start(Opts) ->
   mds_gen_server:start(?MOD, Opts).

stop() ->
   mds_gen_server:stop(?MOD).

on_start(Opts) ->
   SelfOpts = get_param('self', Opts),
   ServerOpts = get_param(mds_server, Opts),
   Port = get_param(port, SelfOpts, ?PORT),
   Db = get_param(db, SelfOpts),
   GtPGroup = get_param(notif, SelfOpts, ?DEF_GT_PGROUP),
   {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {packet, 1}, {reuseaddr, true}, {active, once}]),
   RootDir = get_param(root_dir, ServerOpts),
   WorkingDir = get_param(working_dir, ServerOpts),
   {ok, c} = dets:open_file(c, [{file, filename:join([RootDir, WorkingDir, "devices.dets"])}]),
   log(info, "Started"),
   cleanup_dead_devices(c, GtPGroup),
   {ok, #state{lsocket = ListenSocket, gt_pgroup = GtPGroup, db = Db, devices = c, opts = Opts}, ?TIMEOUT}.

on_stop(Reason, _State) ->
   log(info, "Stopped <~p>.", [Reason]),
   ok.

on_msg(stop, _From, State) ->
   {stop, normal, stopped, State};

on_msg({register, DevName}, {From, _}, State = #state{gt_pgroup = PGroup, devices = C}) ->
   case dets:lookup(C, DevName) of
      [{DevName, From}] -> % already registered by From
         set_online(DevName, PGroup),
         {reply, registered, State, 0};
      [{DevName, _AnotherPid}] -> % already registered by another Pid
         {reply, already_registered, State, 0};
      [] -> % not registered yet
         case dets:match(C, {'$1', From}) of
            [[RegisteredDev]] ->
               change_dev_name(RegisteredDev, DevName, PGroup),
               dets:delete(C, RegisteredDev),
               dets:insert(C, {DevName, From});
            [] ->
               dets:insert(C, {DevName, From}),
               set_online(DevName, PGroup)
         end,
         {reply, registered, State}
   end;

on_msg(_Msg, _Who, State) ->
   {norepy, State, 0}.

on_amsg({log, LogLevel, Text}, State) ->
   log(LogLevel, Text),
   {noreply, State, 0};

on_amsg({log, LogLevel, Format, Params}, State) ->
   log(LogLevel, Format, Params),
   {noreply, State, 0};

on_amsg(_Msg, State) ->
   {norepy, State, 0}.

on_info({'EXIT', From, Reason}, State = #state{gt_pgroup = PGroup, devices = C}) ->
   log(info, "Process ~p exited with reason ~p.~n", [From, Reason]),
   case dets:match(C, {'$1', From}) of
      [[RegisteredDev]] ->
         set_offline(RegisteredDev, PGroup),
         dets:delete(C, RegisteredDev),
         {noreply, State, 0};
      [] ->
         {noreply, State, 0}
   end;

on_info(_Msg, State) ->
   ListenSocket = State#state.lsocket,
   case gen_tcp:accept(ListenSocket, 0) of
      {ok, PeerSocket} ->
         {ok, Addr} = inet:peername(PeerSocket),
         log(info, "Device connected from ~p.", [Addr]),
         process_flag(trap_exit, true),
         gtracker_protocol:start(PeerSocket, [{listener, ?MOD}, {opts, State#state.opts}]),
         {noreply, State, ?TIMEOUT};
      {error, timeout} ->
         {noreply, State, ?TIMEOUT}
   end.

log(LogLevel, Format, Data) ->
   mds_gen_server:log(?MODULE, LogLevel, Format, Data).

log(LogLevel, Text) ->
   mds_gen_server:log(?MODULE, LogLevel, Text).

set_online(DevName, PGroup) ->
   log(info, "Device ~p goes to online.", [DevName]),
   send_pg(PGroup, {online, DevName}).

set_offline(DevName, PGroup) ->
   log(info, "Device ~p goes to offline.", [DevName]),
   send_pg(PGroup, {offline, DevName}).

change_dev_name(OldDevName, NewDevName, PGroup) ->
   log(info, "Device changes its DevName from ~p to ~p.", [OldDevName, NewDevName]),
   send_pg(PGroup, {change_dev_name, OldDevName, NewDevName}).

cleanup_dead_devices(Devices, PGroup) ->
   log(info, "Begin dead devices cleanup."),
   DeadDevNames = dets:foldl(
      fun({DevName, Pid}, AccIn) ->
         case is_process_alive(Pid) of
            false -> [DevName | AccIn];
            true -> AccIn
         end
      end, [], Devices),
   log(info, "Found dead devices: ~p.", [DeadDevNames]),
   lists:foreach(
      fun(DevName) ->
         set_offline(DevName, PGroup),
         dets:delete(Devices, DevName)
      end, DeadDevNames),
   log(info, "End dead devices cleanup.").
