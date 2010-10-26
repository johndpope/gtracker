-module(w).
-export([exec/1, exec/2]).

exec(Format, Params) ->
   Query = io_lib:format(Format, Params),
   exec(Query).

exec(Script) ->
   wf:info(Script),
   wf:wire(Script).
