-module(metrics).

-export([start/0]).

start() ->
   Pid = spawn_link(fun() -> loop() end),
   global:register_name(gt_metrics, Pid).

loop() ->
   receive
      Msg ->
         io:format("~p~n", [Msg]),
         loop()
   end.
