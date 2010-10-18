-module(gtracker_db_pub).

-export(
   [
      new_device/1
      ,select_device/2
      ,register_device/2
      ,unregister_device/2
      ,select_all_devices/2
      ,select_tracks/2
      ,stop_track/3
      ,start_new_track/3
      ,select_triggers/2
      ,rename_track/4
   ]).

new_device(ServerRef) ->
   gen_server:call(ServerRef, new_device).

select_device(ServerRef, DevName) ->
   gen_server:call(ServerRef, {get_device, DevName}).

register_device(ServerRef, DevName) ->
   gen_server:call(ServerRef, {register, DevName, {self(), node()}}).

unregister_device(ServerRef, DevName) ->
   gen_server:call(ServerRef, {unregister, DevName}).

select_all_devices(ServerRef, OnlyActive)->
   gen_server:call(ServerRef, {get_all_devices, OnlyActive}).

select_triggers(ServerRef, DevName) ->
   gen_server:call(ServerRef, {get_triggers, DevName}).

select_tracks(ServerRef, DevName) ->
   gen_server:call(ServerRef, {select_tracks, DevName}).

stop_track(ServerRef, DevName, TrackId) ->
   gen_server:call(ServerRef, {stop_track, DevName, TrackId}).

start_new_track(ServerRef, DevName, TrackName) ->
   gen_server:call(ServerRef, {new_track, DevName, TrackName}).

rename_track(ServerRef, DevName, TrackId, NewTrackName) ->
   gen_server:call(ServerRef, {rename_track, DevName, TrackId, NewTrackName}).
