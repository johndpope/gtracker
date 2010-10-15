-module(gtracker_db_pub).

-export(
   [
      new_device/1
      ,select_device/2
      ,select_all_devices/2
      ,select_tracks/2
      ,start_new_track/2
      ,select_triggers/2
      ,rename_track/3
   ]).

new_device(ServerRef) ->
   gen_server:call(ServerRef, new_device).

select_device(ServerRef, DevName) ->
   gen_server:call(ServerRef, {get_device, DevName}).

set_device_online(ServerRef, DevName, Pid) ->
   gen_server:call(ServerRef, {online, DevName, Pid}).

set_device_offline(ServerRef, DevName) ->
   ServerRef ! {offline, DevName}.

select_all_devices(ServerRef, OnlyActive)->
   gen_server:call(ServerRef, {get_all_devices, OnlyActive}).

select_triggers(ServerRef, DevName) ->
   gen_server:call(ServerRef, {get_triggers, DevName}).

select_tracks(ServerRef, DevName) ->
   gen_server:call(ServerRef, {select_tracks, DevName}).

start_new_track(ServerRef, DevName) ->
   gen_server:call(ServerRef, {new_track, DevName}).

rename_track(ServerRef, DevName, NewTrackName) ->
   gen_server:call(ServerRef, {rename_track, DevName, NewTrackName}).
