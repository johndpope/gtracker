-module(gtracker_app).

-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
   error_logger:add_report_handler(gtracker_alarm),
   {ok, Super} = application:get_env(sup_mod),
   {ok, Servers} = application:get_env(servers),
   apply(Super, start_link, [{servers, Servers}]).

stop(_State) -> ok.
