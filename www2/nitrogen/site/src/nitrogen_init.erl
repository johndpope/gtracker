-module(nitrogen_init).
-export ([init/0]).
-include("db.hrl").

%% Called during application startup.
%% Put other initialization code here.
init() ->
   mysql:start_link(?LINK, "localhost", "gtuser", "Meech20h", "gtracker"),
   application:start(nprocreg).
