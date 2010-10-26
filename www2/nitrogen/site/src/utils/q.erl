-module(q).
-export([exec/1, exec/2]).
-include("db.hrl").

exec(QueryFormat, Params) ->
   Query = io_lib:format(QueryFormat, Params),
   exec(Query).

exec(Query) ->
   wf:info(Query),
   Res = mysql:fetch(?LINK, Query, ?TIMEOUT),
   case Res of
      ?ERROR(_) ->
         bad_query;
      Result ->
         Result
   end.
