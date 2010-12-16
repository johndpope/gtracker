-module(gtracker_db_pub).

-compile({no_auto_import,[register/2]}).

-export(
   [
      get_all_devices/0, get_all_devices/2
      ,get_device/1, get_device/3
      ,update_device/1, update_device/3
      ,register/0, register/2
      ,register/1, register/3
      ,unregister/1, unregister/3
      ,subscribe/1, subscribe/3
      ,unsubscribe/1, unsubscribe/3
      ,new_user/2, new_user/4
      ,update_user/1, update_user/3
      ,get_user/1, get_user/3
      ,authenticate/2, authenticate/4
      ,get_news/0, get_news/2
      ,get_news/1, get_news/3
      ,insert_news/2, insert_news/4
      ,delete_news/1, delete_news/3
      ,get_tracks/1, get_tracks/3
      ,new_track/2, new_track/4
      ,get_triggers/1, get_triggers/3
   ]).

-include("common_recs.hrl").
-include("common_defs.hrl").

-define(MAX_CALL_TIMEOUT, 30000).

% get_all_devices(Db, Timeout) -> [Device] | error
%  Db = registereg Db name
%  Timeout - call timeout
%  Device = #device, See device record in gtracker/server/include/common_defs.hrl for details
get_all_devices(Db, Timeout) ->
   gen_server:call(Db, get_all_devices, Timeout).
get_all_devices() ->
   get_all_devices(?db_ref, ?MAX_CALL_TIMEOUT).

% get_device(Db, DevName, Timeout) -> Device() | error
%  Db = registereg Db name
%  Timeout - call timeout
%  DevName = String()
get_device(Db, DevName, Timeout) ->
   gen_server:call(Db, {get_device, DevName}, Timeout).
get_device(DevName) ->
   get_device(?db_ref, DevName, ?MAX_CALL_TIMEOUT).

% update_device(Db, Device(), Timeout) -> Device() | error
%  Db = registereg Db name
%  Timeout - call timeout
update_device(Db, Device, Timeout) ->
   gen_server:call(Db, {update_device, Device}, Timeout).
update_device(Device) ->
   update_device(?db_ref, Device, ?MAX_CALL_TIMEOUT).

% register(Db, Timeout) -> Device() | error
%  Db = registereg Db name
%  Timeout - call timeout
%  registers a new device
register(Db, Timeout) ->
   gen_server:call(Db, register, Timeout).
register() ->
   register(?db_ref, ?MAX_CALL_TIMEOUT).

% reister(Db, DevName, Timeout) -> Device() | no_such_device | already_registered | error
%  Db = registereg Db name
%  Timeout - call timeout
%  registers a existing device
register(Db, DevName, Timeout) ->
   gen_server:call(Db, {register, DevName}, Timeout).
register(DevName) ->
   register(?db_ref, DevName, ?MAX_CALL_TIMEOUT).

% unregister(Db, DevName, Timeout) -> unregistered | no_such_device | error | wrong_owner
%  Db = registereg Db name
%  Timeout - call timeout
unregister(Db, DevName, Timeout) ->
   Device =  get_device(DevName),
   gtracker_track_pub:close(Device#device.links#links.track),
   gen_server:call(Db, {unregister, DevName}, Timeout).
unregister(DevName) ->
   unregister(?db_ref, DevName, ?MAX_CALL_TIMEOUT).

% subscribe(Db, DevName, Timeout) -> ok | atom()
%  Db = registereg Db name
%  Timeout - call timeout
%   DevName = String()
subscribe(Db, DevName, Timeout) ->
   gen_server:call(Db, {subscribe, DevName}, Timeout).
subscribe(DevName) ->
   subscribe(?db_ref, DevName, ?MAX_CALL_TIMEOUT).

% unsubscribe(Db, DevName, Timeout) -> ok | atom()
%  Db = registereg Db name
%  Timeout - call timeout
%   DevName = String()
unsubscribe(Db, DevName, Timeout) ->
   gen_server:call(Db, {unsubscribe, DevName}, Timeout).
unsubscribe(DevName) ->
   unsubscribe(?db_ref, DevName, ?MAX_CALL_TIMEOUT).

% new_user(Db, UserName, Password, Timeout) -> User | already_exists | error
%  Db = registereg Db name
%  Timeout - call timeout
%  UserName = String(), e.g. demo@demo.org
%  Password = String()
%  User = #user, see common_defs.hrl for details
new_user(Db, UserName, Password, Timeout) ->
   gen_server:call(Db, {new_user, UserName, Password}, Timeout).
new_user(UserName, Password) ->
   new_user(?db_ref, UserName, Password, ?MAX_CALL_TIMEOUT).

% update_user(Db, User(), Timeout) -> User() | no_such_user | error
%  Db = registereg Db name
%  Timeout - call timeout
update_user(Db, User, Timeout) ->
   gen_server:call(Db, {update_user, User}, Timeout).
update_user(User) ->
   update_user(?db_ref, User, ?MAX_CALL_TIMEOUT).

% get_user(Db, UserName, Timeout) -> User()
%  Db = registereg Db name
%  Timeout - call timeout
%  UserName = String()
get_user(Db, UserName, Timeout) ->
   gen_server:call(Db, {get_user, UserName}, Timeout).
get_user(UserName) ->
   get_user(?db_ref, UserName, ?MAX_CALL_TIMEOUT).

% authenticate(Db, UserName, Password, Timeout) -> User() | rejected | error
%  Db = registereg Db name
%  Timeout - call timeout
%  UserName = String()
%  Password = String()
authenticate(Db, UserName, Password, Timeout) ->
   gen_server:call(Db, {login, UserName, Password}, Timeout).
authenticate(UserName, Password) ->
   gen_server:call(?db_ref, {login, UserName, Password}, ?MAX_CALL_TIMEOUT).

% get_tracks(Db, DevName, Timeout) -> Tracks | error
%  Db = registereg Db name
%  Timeout - call timeout
%  Tracks = [Track]
%  Track = #track. See common_defs.hrl for details
get_tracks(Db, DevName, Timeout) ->
   gen_server:call(Db, {get_tracks, DevName}, Timeout).
get_tracks(DevName) ->
   get_tracks(?db_ref, DevName, ?MAX_CALL_TIMEOUT).

% get_triggers(Db, DevName, Timeout) -> Triggers | error
%  Db = registereg Db name
%  Timeout - call timeout
%  Triggers = [Trigger]
%  Trigger = #trigger. See common_defs.hrl for details
get_triggers(Db, DevName, Timeout) ->
   gen_server:call(Db, {get_triggers, DevName}, Timeout).
get_triggers(DevName) ->
   get_triggers(?db_ref, DevName, ?MAX_CALL_TIMEOUT).

get_news(Db, Timeout) ->
   gen_server:call(Db, {get_news, undef}, Timeout).
get_news() ->
   get_news(?db_ref, ?MAX_CALL_TIMEOUT).

get_news(Db, UpToDate, Timeout) ->
   gen_server:call(Db, {get_news, UpToDate}, Timeout).
get_news(UpToDate) ->
   get_news(?db_ref, UpToDate, ?MAX_CALL_TIMEOUT).

insert_news(Db, Date, Text, Timeout) ->
   gen_server:call(Db, {insert_news, Date, Text}, Timeout).
insert_news(Date, Text) ->
   insert_news(?db_ref, Date, Text, ?MAX_CALL_TIMEOUT).

delete_news(Db, NewsRef, Timeout) ->
   gen_server:call(Db, {delete_news, NewsRef}, Timeout).
delete_news(NewsRef) ->
   delete_news(?db_ref, NewsRef, ?MAX_CALL_TIMEOUT).

new_track(Db, Device, Force, Timeout) ->
   case gen_server:call(Db, {new_track, Device#device.name, Force, []}, Timeout) of
      device_not_registered ->
         device_not_registered;
      Track when is_record(Track, track) ->
         TrackPid = gtracker_track_pub:open(Track),
         Links = Device#device.links,
         update_device(Db, Device#device{links = Links#links{track = TrackPid}, current_track = Track#track.id}, Timeout),
         TrackPid;
      {already_has_active_track, TrackPid} ->
         TrackPid
   end.
new_track(Device, Force) ->
   new_track(?db_ref, Device, Force, ?MAX_CALL_TIMEOUT).
