-module(nitrogen_init).
-export ([init/0]).

%% Called during application startup.
%% Put other initialization code here.
init() ->
   net_adm:ping('gtracker_db@127.0.0.1'),
   application:start(nprocreg).
