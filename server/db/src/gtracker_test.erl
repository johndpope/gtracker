-module(gtracker_test).

-include_lib("eunit/include/eunit.hrl").

start_test() ->
   application:start(gtracker_db).

stop_test() ->
   application:stop(gtracker_db).
