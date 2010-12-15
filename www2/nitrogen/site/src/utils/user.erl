-module(user).
-include_lib("nitrogen/include/wf.hrl").
-include_lib("gtracker_db_pub/include/common_recs.hrl").
-export([
      save_into_session/1,
      devices/0,
      admin/0,
      map_type/0,
      update/1,
      device_list/0
   ]).

save_into_session(UserInfo) ->
   wf:user(UserInfo#user.name),
   wf:session(user_info, UserInfo).

devices() ->
   UserInfo = wf:session(user_info),
   UserInfo#user.devices.

admin() ->
   UserInfo = wf:session(user_info),
   UserInfo#user.is_admin.

map_type() ->
   UserInfo = wf:session(user_info),
   UserInfo#user.map_type.

update(UserInfo) ->
   case gtracker_db_pub:update_user(UserInfo) of
      no_such_user ->
         error;
      error ->
         error;
      SavedUserInfo ->
         SavedUserInfo
   end.

device_list() ->
   device_list(wf:session(device_list)).

device_list(undefined) ->
   UserInfo = wf:session(user_info),
   Functor = fun(Name) ->
         case gtracker_db_pub:get_device(Name) of
            error ->
               {Name, error};
            Device ->
               Device
         end
   end,
   Devices = lists:map(Functor, UserInfo#user.devices),
   wf:session(device_list, Devices),
   Devices;

device_list(Devices) ->
   Devices.
