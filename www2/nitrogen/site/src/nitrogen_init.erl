-module(nitrogen_init).
-export ([init/0]).
-include("db.hrl").

%% Called during application startup.
%% Put other initialization code here.
init() ->
   net_adm:ping('fake_server@127.0.0.1'),
   mysql:start_link(?LINK, "localhost", "gtuser", "Meech20h", "gtracker"),
   application:start(nprocreg).
