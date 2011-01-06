-module(subs).

-export([start/1, stop/1]).

-include("../../include/common_recs.hrl").

start(DevName) ->
   spawn_link(fun() -> subscribe(DevName) end).

stop(Pid) ->
   Pid ! unsubscribe.

subscribe(DevName) ->
   gtracker_pub:subscribe(DevName),
   loop(DevName).


loop(DevName) ->
   receive
      Msg when is_tuple(Msg) ->
         io:format("~p~n", [Msg]),
         loop(DevName);
      unsubscribe ->
         gtracker_pub:unsubscribe(DevName),
         stopped
   end.
