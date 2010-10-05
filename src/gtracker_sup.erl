-module(gtracker_sup).


-behaviour(supervisor).
-import(mds_utils, [get_param/2, get_param/3]).

-export([start_link/1, init/1, start_in_shell/0]).

start_in_shell() ->
   {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, [[{root_dir, "/tmp"}]]),
   unlink(Pid).

start_link(Opts) ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, Opts).

init({servers, Servers})->
   NormEnv = fun(Key) ->
      case application:get_env(Key) of
         undefined ->
            dict:new();
         {ok, Opts} ->
            dict:from_list(Opts)
      end
   end,
   ServerOpts = NormEnv(mds_server),
   LoggerOpts = NormEnv(mds_logger),

   RootDir = get_param(root_dir, dict:to_list(ServerOpts), "."),
   filelib:ensure_dir(filename:join(RootDir, "app/.")),

   AlarmModOpts = normalize_opts(get_param(gtracker_alarm, Servers), ServerOpts, LoggerOpts),
   ok = error_logger:add_report_handler(gtracker_alarm, AlarmModOpts),

   SrvList = init_server(Servers, ServerOpts, LoggerOpts, 1),
   {
      ok,
      {
         {one_for_one, 3, 10}, SrvList
      }
   }.

init_server([], _, _, _) ->
   [];
init_server([{gtracker_alarm, _}|Servers], ServerOpts, LoggerOpts, TagNum) -> % skip gtracker_alarm module. This is the special module
   init_server(Servers, ServerOpts, LoggerOpts, TagNum);
init_server([{ServerName, Opts}|Servers], ServerOpts, LoggerOpts, TagNum) ->
   NormServerOpts = normalize_opts(Opts, ServerOpts, LoggerOpts),
   Tag = mktag(TagNum),
   [{Tag, {ServerName, start, [NormServerOpts]}, permanent, 10000, worker, [ServerName]}
      | init_server(Servers, ServerOpts, LoggerOpts, TagNum + 1)].

mktag(Num) ->
   erlang:list_to_atom(lists:flatten(io_lib:format("gtracker_tag_~p", [Num]))).

normalize_opts(ModOpts, EnvServerOpts, EnvLoggerOpts) ->
   SelfServerOpts = dict:from_list(get_param(mds_server, ModOpts, [])),
   SelfLoggerOpts = dict:from_list(get_param(mds_logger, ModOpts, [])),
   NormServerOpts = dict:merge(fun(_Key, A, _B) -> A end, SelfServerOpts, EnvServerOpts),
   NormLoggerOpts = dict:merge(fun(_Key, A, _B) -> A end, SelfLoggerOpts, EnvLoggerOpts),
   NewModOpts = dict:store(mds_server, dict:to_list(NormServerOpts), dict:from_list(ModOpts)),
   dict:to_list(dict:store(mds_logger, dict:to_list(NormLoggerOpts), NewModOpts)).
