-module(gtracker_mnesia).

-behaviour(mds_gen_server).

-export([start/1, stop/0, on_start/1, on_stop/2, on_msg/3, on_amsg/2, on_info/2]).

-import(mds_utils, [get_param/2, get_param/3]).
-import(gtracker_common, [gen_dev_name/0, join_pg/2, binary_to_hex/1]).

-include("common_defs.hrl").

-define(MOD, {global, gtracker_db}).
-define(DEF_FAILOVER_PERIOD, 10).
-define(DEF_TABLE_DIR, ".").
-define(DEF_KEY_POS, 2).

-record(state, {}).

%=======================================================================================================================
%  public exports
%=======================================================================================================================
start(Opts) ->
   mds_gen_server:start(?MOD, Opts).

stop() ->
   mds_gen_server:stop(?MOD).

%=======================================================================================================================
%  callbacks
%=======================================================================================================================
on_start(Opts) ->
   SelfOpts = get_param(self, Opts),
   PGroup = get_param(notif, SelfOpts, ?DEF_GT_PGROUP),
   crypto:start(),
   mnesia:start(),
   Res = (catch mnesia:table_info(device, all)),
   case Res of
     {'EXIT',{aborted,{no_exists,device,all}}} ->
        create_schema();
     _ -> ok
   end,
   log(info, "Mnesia started."),
   join_pg(PGroup, self()),
   {ok, #state{}}.

on_stop(Reason, _State) ->
   crypto:stop(),
   mnesia:stop(),
   log(info, "Mnesia stopped."),
   log(info, "Stopped <~p>.", [Reason]),
   ok.

on_msg(stop, _From, State) ->
   {stop, normal, stopped, State};

on_msg(get_device, _From, State) ->
   F = fun(Fun) ->
         DevName = gen_dev_name(),
         log(debug, "Device generated ~p.", [DevName]),
         case mnesia:dirty_index_read(device, DevName, #device.name) of
            [] ->
               log(debug, "Store device ~p", [DevName]),
               Ref = binary_to_hex(erlang:md5(erlang:list_to_binary(DevName))),
               mnesia:dirty_write(#device{id = make_ref(), name = DevName, alias = DevName, reference = Ref, registered = now()}),
               {DevName, Ref};
            [#device{name = DevName}] ->
               Fun(Fun)
         end
      end,
   try F(F) of
      {DevName, Ref} ->
         {reply, {DevName, Ref}, State}
   catch
      _:Err ->
         log(error, "get_device/0 failed: Error = ~p", [Err]),
         {reply, error, State}
   end;

on_msg({get_device, DevName}, _From, State) ->
   log(debug, "get_device. DevName: ~p, State: ~p", [DevName, dump_state(State)]),
   try mnesia:dirty_index_read(device, DevName, #device.name) of
      [] ->
         {reply, no_device, State};
      [Device = #device{name = DevName}] ->
         {reply, Device, State}
   catch
      _:Err ->
         log(error, "get_device/1 failed: Error = ~p", [Err]),
         {reply, error, State}
   end;

on_msg({get_triggers, DevName}, _From, State) ->
   try mnesia:dirty_index_read(device, DevName, #device.name) of
      [] ->
         {reply, no_triggers, State};
      [#device{triggers = T}] ->
         {reply, T, State}
   catch
      _:Err ->
         log(error, "get_triggers/1 failed: Error = ~p", [Err]),
         {reply, error, State}
   end;

on_msg(Msg, _From, State) ->
   log(error, "Unknown sync message ~p.", [Msg]),
   {noreply, State}.

on_amsg(Msg, State) ->
   log(error, "Unknown async message ~p.", [Msg]),
   {noreply, State}.

%set device online
on_info(?MSG(_From, _GroupName, {online, DevName}), State) ->
   log(debug, "online. DevName: ~p, State: ~p", [DevName, dump_state(State)]),
   try mnesia:dirty_read({device, DevName}) of
      [] ->
         log(error, "Unable to set device ~p online. Device not found.", [DevName]),
         {noreply, State};
      [Device] ->
         mnesia:dirty_write(Device#device{online = true}),
         {noreply, State}
   catch
      _:Err ->
         log(error, "set_online failed: Error = ~p", [Err]),
         {noreply, State}
   end;

%set device offline
on_info(?MSG(_From, _GroupName, {offline, DevName}), State) ->
   log(debug, "offline. DevName: ~p, State: ~p", [DevName, dump_state(State)]),
   try mnesia:dirty_read({device, DevName}) of
      [] ->
         log(error, "Unable to set device ~p offline. Device not found.", [DevName]),
         {noreply, State};
      [Device] ->
         mnesia:dirty_write(Device#device{online = false}),
         {noreply, State}
   catch
      _:Err ->
         log(error, "set_offline failed: Error = ~p", [Err]),
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
create_schema() ->
   mnesia:stop(),
   mnesia:create_schema([]),
   mnesia:start(),
   mnesia:create_table(device, [{disc_copies, [node()]}, {index, [name]}, {type, ordered_set}, {attributes, record_info(fields,device)}]),
   mnesia:create_table(user, [{disc_copies, [node()]}, {type, ordered_set}, {attributes, record_info(fields, user)}]).

dump_state(State) ->
   State.

%=======================================================================================================================
%  unit testing facilities
%=======================================================================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_device_test() ->
   Pid = gtracker_edb:start([]),
   timer:sleep(100),
   {DevName, _} = gen_server:call(?MOD, get_device),
   Device = gen_server:call(?MOD, {get_device, DevName}),
   ?assertEqual(DevName, Device#device.name).

-endif.
