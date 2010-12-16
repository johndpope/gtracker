-module(client).
-export([warning/1, warning/2, error/1, error/2]).

warning(Format, Params) ->
   Query = wf:f(Format, Params),
   warning(Query).

warning(Message) ->
   Query = wf:f("alert('Warning: ~s');", [Message]),
   wf:wire(Query).

error(Format, Params) ->
   Query = wf:f(Format, Params),
   client:error(Query).

error(Message) ->
   Query = wf:f("alert('Error: ~s');", [Message]),
   wf:wire(Query).
