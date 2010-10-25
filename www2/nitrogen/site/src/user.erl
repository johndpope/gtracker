-module(user).
-include_lib("nitrogen/include/wf.hrl").
-include("db.hrl").
-compile(export_all).

load_settings_into_session(UserID) ->
   % load user settings
   case q:exec(?USER_LOAD_SETTINGS, [UserID]) of
      ?RESULT([[Admin,MapID]]) ->
         wf:session(admin, Admin),
         wf:session(map_id, integer_to_list(MapID)),
         wf:info("Settings saved into session (UserID=~w, Admin=~w, MapID=~w)", [UserID, Admin, MapID]);

      ?RESULT([]) ->
         wf:error("Failed to save settings into session, user not exist (UserID=~w)", [UserID]);

      bad_query ->
         wf:error("Internal error (UserID=~w)", [UserID])
   end.

load_devices_into_session(UserID) ->
   % load device list of user 
   ?RESULT(List) = q:exec(?DEVICE_LIST, [UserID]),
   wf:session(devices, List).

id() ->
   wf:session(user_id).

id(UserID) ->
   wf:session(user_id, UserID).
