-module(gtracker_mnesia).

-behaviour(mds_gen_server).

-export([start/1, stop/0, on_start/1, on_stop/2, on_msg/3, on_amsg/2, on_info/2]).

-import(mds_utils, [get_param/2, get_param/3]).
-import(gtracker_common, [gen_dev_name/0, binary_to_hex/1, get_best_process/1]).

-include("common_defs.hrl").

-define(mod, {global, gtracker_db}).
-define(log_error(MethodName), log(error, "~s failed: ~p. Msg = ~p, State = ~p, Stack trace = ~p", [MethodName, Err,
         Msg, dump_state(State), erlang:get_stacktrace()])).
-define(def_triggers, {global, gtracker_triggers}).

-record(state, {triggers = undef}).

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
   Triggers = get_param(notif, SelfOpts, ?def_triggers),
   crypto:start(),
   mnesia:start(),
   Res = (catch mnesia:table_info(device, all)),
   case Res of
     {'EXIT',{aborted,{no_exists,device,all}}} ->
        create_schema();
     _ -> ok
   end,
   log(info, "Mnesia started."),
   {ok, #state{triggers = Triggers}}.

on_stop(Reason, _State) ->
   crypto:stop(),
   mnesia:stop(),
   log(info, "Mnesia stopped."),
   log(info, "Stopped <~p>.", [Reason]),
   ok.

on_msg(stop, _From, State) ->
   {stop, normal, stopped, State};

on_msg(Msg = register, {Pid, _}, State) ->
   log(debug, "register. State: ~p", [dump_state(State)]),
   F = fun(Fun) ->
         DevName = gen_dev_name(),
         log(debug, "Device generated ~p.", [DevName]),
         case mnesia:dirty_read(device, DevName) of
            [] ->
               log(debug, "Store device ~p", [DevName]),
               Ref = binary_to_hex(erlang:md5(erlang:list_to_binary(DevName))),
               Device = #device
               {
                  name = DevName,
                  alias = DevName,
                  reference = Ref,
                  online = true,
                  links = #links{owner = Pid}
               },
               mnesia:dirty_write(Device),
               Device;
            [#device{name = DevName}] ->
               Fun(Fun)
         end
      end,
   try F(F) of
      Device ->
         {reply, Device, State}
   catch
      _:Err ->
         ?log_error("register/0"),
         {reply, error, State}
   end;

on_msg(Msg = {register, DevName}, {Pid, _}, State = #state{triggers = Triggers}) ->
   log(debug, "register(~p). State: ~p", [DevName, dump_state(State)]),
   F = fun() ->
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
            case rpc:call(erlang, is_process_alive, [node(Owner), Owner]) of
               true ->
                  {reply, already_registered, State};
               False ->
                  log(debug, "is_process_alive(~p): ~p", [Owner, False]),
                  NewDevice = activate_device(Device, Pid, Triggers),
                  mnesia:dirty_write(NewDevice),
                  {reply, NewDevice, State}
            end
      end
   end,
   try F()
   catch
      _:Err ->
        ?log_error("register/1"),
        {reply, error, State}
   end;

on_msg(Msg = {unregister, DevName}, {Pid, _}, State) ->
   log(debug, "unregister(~p). State: ~p", [DevName, dump_state(State)]),
   F = fun() ->
      case mnesia:dirty_read(device, DevName) of
         [] ->
            {reply, no_such_device, State};
         [#device{links = #links{owner = undef}, online = false}] ->
            {reply, unregistered, State};
         [Device = #device{links = #links{owner = Owner}}] when is_pid(Owner) andalso (Pid == Owner) ->
            mnesia:dirty_write(Device#device{links = #links{}, online = false}),
            {reply, unregistered, State};
         [Device = #device{links = #links{owner = Owner}}] ->
            case rpc:call(erlang, is_process_alive, [node(Owner), Owner]) of
               true ->
                  {reply, wrong_owner, State};
               False ->
                  log(debug, "is_process_alive(~p): ~p", [Owner, False]),
                  mnesia:dirty_write(Device#device{links = #links{}, online = false}),
                  {reply, unregistered, State}
            end
         end
      end,
   try F()
   catch
      _:Err ->
         ?log_error("unregister/1"),
         {reply, error, State}
   end;

on_msg(Msg = {new_user, UserName, Password}, _From, State) ->
   log(debug, "new_user(~p, ~p). State: ~p", [UserName, Password, dump_state(State)]),
   F = fun() ->
      case mnesia:dirty_read(user, UserName) of
         [] ->
            User = #user{name = UserName, password = erlang:md5(Password)},
            mnesia:dirty_write(User),
            {reply, User, State};
         _User ->
            {reply, already_exists, State}
         end
      end,
   try F()
   catch
      _:Err ->
         ?log_error("new_user/2"),
         {reply, error, State}
   end;

on_msg(Msg = {login, UserName, Password}, _From, State) ->
   log(debug, "login(~p, ~p). State: ~p", [UserName, Password, dump_state(State)]),
   F = fun() ->
      case mnesia:dirty_read(user, UserName) of
         [] ->
            {reply, not_exists, State};
         [User = #user{name = U, password = P}] ->
            case (U == UserName) andalso (P == erlang:md5(Password)) of
               true ->
                  OnlineUser = User#user{online = true},
                  mnesia:dirty_write(OnlineUser),
                  {reply, OnlineUser, State};
               false ->
                  {reply, rejected, State}
            end
      end
   end,
   try F()
   catch
      _:Err ->
         ?log_error("login/2"),
         {reply, error, State}
   end;
on_msg(Msg = {logout, UserName}, _From, State) ->
   log(debug, "logout(~p). State: ~p", [UserName, dump_state(State)]),
   F = fun() ->
      case mnesia:dirty_read(user, UserName) of
         [] ->
            {reply, rejected, State};
         [User = #user{name = UserName}] ->
            mnesia:dirty_write(User#user{online = false}),
            {reply, ok, State}
      end
   end,
   try F()
   catch
      _:Err ->
         ?log_error("logout/2"),
         {reply, error, State}
   end;

%on_msg({get_triggers, DevName}, _From, State) ->
%   try mnesia:dirty_index_read(device, DevName, #device.name) of
%      [] ->
%         {reply, no_triggers, State};
%      [#device{triggers = T}] ->
%         {reply, T, State}
%   catch
%      _:Err ->
%         log(error, "get_triggers/1 failed: Error = ~p", [Err]),
%         {reply, error, State}
%   end;

on_msg(Msg, _From, State) ->
   log(error, "Unknown sync message ~p.", [Msg]),
   {noreply, State}.

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

%=======================================================================================================================
%  tools
%=======================================================================================================================
create_schema() ->
   mnesia:stop(),
   mnesia:create_schema([]),
   mnesia:start(),
   mnesia:create_table(device, [{disc_copies, [node()]}, {type, ordered_set}, {attributes, record_info(fields,device)}]),
   mnesia:create_table(trigger, [{disc_copies, [node()]}, {type, ordered_set}, {attributes, record_info(fields, trigger)}]),
   mnesia:create_table(user, [{disc_copies, [node()]}, {type, ordered_set}, {attributes, record_info(fields, user)}]).

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

%=======================================================================================================================
%  unit testing facilities
%=======================================================================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_device_test() ->
   Pid = gtracker_edb:start([]),
   timer:sleep(100),
   {DevName, _} = gen_server:call(?mod, get_device),
   Device = gen_server:call(?mod, {get_device, DevName}),
   ?assertEqual(DevName, Device#device.name).

-endif.
