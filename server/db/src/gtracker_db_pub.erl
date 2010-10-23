-module(gtracker_db_pub).

-export(
   [
      get_devices/1
      ,register/1
      ,register/2
      ,unregister/2
      ,new_user/3
      ,login/3
      ,logout/2
   ]).

-define(db_ref, {global, gtracker_db}).

get_devices() ->
   gen_server:call(?db_ref, get_devices).

register() ->
   gen_server:call(?db_ref, register).

register(DevName) ->
   gen_server:call(?db_ref, {register, DevName}).

unregister(DevName) ->
   gen_server:call(?db_ref, {unregister, DevName}).

new_user(UserName, Password) ->
   gen_server:call(?db_ref, {new_user, UserName, Password}).

login(UserName, Password) ->
   gen_server:call(?db_ref, {login, UserName, Password}).

logout(UserName) ->
   gen_server:call(?db_ref, {logout, UserName}).
