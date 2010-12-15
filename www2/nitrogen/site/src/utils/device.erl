-module(device).
-include_lib("nitrogen/include/wf.hrl").
-include_lib("gtracker_db_pub/include/common_recs.hrl").
-compile(export_all).

update(Device) ->
   case gtracker_db_pub:update_device(Device) of
      unregistered->
         error;
      no_such_device ->
         error;
      error ->
         error;
      wrong_owner ->
         error;
      SavedDevice ->
         SavedDevice
   end.
