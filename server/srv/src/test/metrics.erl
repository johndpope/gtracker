-module(metrics).

-export([start/0]).

start() ->
   Pid = spawn_link(fun() -> init() end),
   global:register_name(gt_metrics, Pid).

init() ->
   {ok, H} = file:open("/tmp/metrics.log", [write]),
   loop(H).

loop(H) ->
   receive
      stop ->
         io:format(H, "Buy!");
      Msg ->
         io:format(H, "~p~n", [Msg]),
         loop(H)
   end.
