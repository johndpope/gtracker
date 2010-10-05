-module(gtracker_table).

-define(DEF_DUMP_TIMEOUT, 1000).
-define(DEF_WORKING_DIR, ".").
-define(DEF_KEY_POS, 1).
-define(WAIT_TIMEOUT, 5000).
-define(DEF_AUTO_UNLOAD, 600000).

-record(state, {full_path, log_callback, table_name, key_pos, dump_timeout, auto_unload, table_ref = undef, dump = false}).

-export([open/2, open/3, insert/2, ainsert/2, lookup/2, last/1, match/2, delete/2, unload/1, info/1, info/2, close/1]).

-export([init/3]).

-import(mds_utils, [get_param/2, get_param/3]).

%% open(TableName, Settings) -> TableRef
%%    TableName = atom()
%%    Settings = [Setting]
%%    Setting = {working_dir, String()}, {dump_timeout, Int()}, {key_pos, Int{}}, {auto_unload, Int()}
%%      working_dir - absolute or relative path, where table file will be created. Default = "."
%%      dump_timeout - dump table period in milliseconds. Default = 1000
%%      key_pos - position of a key on tuple. Default = 1
%%      auto_unload - period in milliseconds, when table automatically unloaded if there is no activities. Default = 10 min.
open(TableName, Settings) ->
   Pid = spawn(?MODULE, init, [TableName, Settings, undef]),
   true = register(TableName, Pid),
   TableName.

open(TableName, Settings, LogCallback) ->
   Pid = spawn(?MODULE, init, [TableName, Settings, LogCallback]),
   true = register(TableName, Pid),
   TableName.

init(TableName, Settings, LogCallback) ->
   FileName = atom_to_list(TableName) ++ ".data",
   KeyPos = get_param(key_pos, Settings, ?DEF_KEY_POS),
   WorkingDir = get_param(working_dir, Settings, ?DEF_WORKING_DIR),
   FullPath = filename:join(WorkingDir, FileName),
   DumpTimeout = get_param(dump_timeout, Settings, ?DEF_DUMP_TIMEOUT), %% TODO: check for valid value
   AutoUnload = get_param(auto_unload, Settings, ?DEF_AUTO_UNLOAD),    %% TODO: check for valid value
   State = #state{
         full_path = FullPath,
         log_callback = LogCallback,
         table_name = TableName,
         key_pos = KeyPos,
         dump_timeout = DumpTimeout,
         auto_unload = AutoUnload},
   LState = load_table(State),
   loop(LState).

loop(State = #state{log_callback = LogCallback, table_name = TableName}) ->
   receive
      dump when State#state.table_ref /= undef  ->
         dump_table(State),
         loop(State#state{dump = false});
      {insert, Pid, Data} ->
         NewState = load_table(State),
         true = ets:insert(NewState#state.table_ref, Data),
         Pid ! {TableName, ok},
         case State#state.dump of
            false ->
               erlang:send_after(State#state.dump_timeout, self(), dump),
               loop(NewState#state{dump = true});
            true ->
               loop(State)
         end;
      {ainsert, Pid, Data} ->
         Pid ! {TableName, ok},
         NewState = load_table(State),
         true = ets:insert(NewState#state.table_ref, Data),
         case State#state.dump of
            false ->
               erlang:send_after(State#state.dump_timeout, self(), dump),
               loop(NewState#state{dump = true});
            true ->
               loop(State)
         end;
      {delete, Pid, Key} ->
         NewState = load_table(State),
         ets:delete(NewState#state.table_ref, Key),
         Pid ! {TableName, ok},
         case State#state.dump of
            false ->
               erlang:send_after(State#state.dump_timeout, self(), dump),
               loop(NewState#state{dump = true});
            true ->
               loop(State)
         end;
      {lookup, Pid, Key} ->
         NewState = load_table(State),
         Res = ets:lookup(NewState#state.table_ref, Key),
         Pid ! {TableName, Res},
         loop(NewState);
      {last, Pid} ->
         NewState = load_table(State),
         LastKey = ets:last(NewState#state.table_ref),
         Res = ets:lookup(NewState#state.table_ref, LastKey),
         Pid ! {TableName, Res},
         loop(NewState);
      {match, Pid, Pattern} ->
         NewState = load_table(State),
         Res = ets:match(NewState#state.table_ref, Pattern),
         Pid ! {TableName, Res},
         loop(NewState);
      {unload, Pid}  when State#state.table_ref == undef ->
         Pid ! {TableName, ok},
         loop(State);
      {unload, Pid} ->
         NewState = load_table(State),
         dump_table(NewState),
         ets:delete(NewState#state.table_ref),
         Pid ! {TableName, ok},
         loop(NewState#state{table_ref = undef});
      {info, Pid} ->
         NewState = load_table(State),
         Pid ! {TableName, ets:info(NewState#state.table_ref)},
         loop(NewState);
      {info, Item, Pid} ->
         NewState = load_table(State),
         Pid ! {TableName, ets:info(NewState#state.table_ref, Item)},
         loop(NewState);
      {close, Pid} ->
         NewState = load_table(State),
         dump_table(NewState),
         ets:delete(NewState#state.table_ref),
         Pid ! {TableName, ok};
      Unk ->
         log(LogCallback, error, "~p: Unknown command ~p.", [TableName, Unk])
   after State#state.auto_unload ->
      case State#state.table_ref of
         undef ->
            loop(State);
         _ ->
            log(LogCallback, debug, "~p: unloading table ...", [TableName]),
            dump_table(State),
            ets:delete(State#state.table_ref),
            loop(State#state{table_ref = undef})
      end
   end.

close(TableRef) ->
   TableRef ! {close, self()},
   wait(TableRef).

insert(TableRef, Obj) ->
   TableRef ! {insert, self(), Obj},
   wait(TableRef).

ainsert(TableRef, Obj) ->
   TableRef ! {ainsert, self(), Obj}.

lookup(TableRef, Key) ->
   TableRef ! {lookup, self(), Key},
   wait(TableRef).

last(TableRef) ->
   TableRef ! {last, self()},
   wait(TableRef).

match(TableRef, Pattern) ->
   TableRef ! {match, self(), Pattern},
   wait(TableRef).

delete(TableRef, Key) ->
   TableRef ! {delete, self(), Key},
   wait(TableRef).

unload(TableRef) ->
   TableRef ! {unload, self()},
   wait(TableRef).

info(TableRef) ->
   TableRef ! {info, self()},
   wait(TableRef).

info(TableRef, Item) ->
   TableRef ! {info, Item, self()},
   wait(TableRef).

wait(TableRef) ->
   receive
      {TableRef, Res} ->
         Res
   after ?WAIT_TIMEOUT  ->
      case is_process_alive(erlang:whereis(TableRef)) of
         true ->
            timeout;
         false ->
            no_such_table
      end
   end.

dump_table(#state{table_ref = undef}) ->
   ok;
dump_table(#state{log_callback = LogCallback, table_name = TableName, table_ref = TableRef, full_path = FullPath}) ->
   log(LogCallback, debug, "~p: staring to dump into ~s ...", [TableName, FullPath]),
   ok = ets:tab2file(TableRef, FullPath, [{extended_info, [object_count, md5sum]}]).

load_table(State = #state{table_name = TableName, log_callback = LogCallback, table_ref = TableRef, full_path =
      FullPath, key_pos = KeyPos}) ->
   case ets:info(TableRef) of
      Res when is_list(Res) ->
         State;
      undefined ->
         case filelib:is_file(FullPath) of
            true ->
               log(LogCallback, debug, "~p: loading table from ~s ...", [TableName, FullPath]),
               {ok, NewTableRef} = ets:file2tab(FullPath, [{verify, true}]),
               State#state{table_ref = NewTableRef};
            false ->
               log(LogCallback, debug, "~p: creating table ...", [TableName]),
               NewTableRef = ets:new(TableName, [ordered_set, {keypos, KeyPos}]),
               State#state{table_ref = NewTableRef}
         end
   end.

log(null, _LogLevel, _Format, _Data) ->
   ok;
log(undef, LogLevel, Format, Data) ->
   io:format("~p: " ++ Format ++ "~n", [LogLevel | Data]);
log(LogCallback, LogLevel, Format, Data) ->
   LogCallback(LogLevel, Format, Data).
