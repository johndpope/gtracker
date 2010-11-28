-module(gtracker_db_pub).

-export(
   [
      get_all_devices/0
      ,get_device/1
      ,update_device/1
      ,register/0
      ,register/1
      ,unregister/1
      ,new_user/2
      ,update_user/1
      ,get_user/1
      ,authenticate/2
      ,get_tracks/1
      ,get_triggers/1
      ,get_news/0
      ,get_news/1
      ,insert_news/2
      ,delete_news/1
      ,new_track/2
   ]).

-include("common_recs.hrl").

-define(db_ref, {global, gtracker_db}).

% get_all_devices() -> [Device] | error
%  Device = #device, See device record in gtracker/server/include/common_defs.hrl for details
get_all_devices() ->
   gen_server:call(?db_ref, get_all_devices).

% get_device(DevName) -> Device() | error
%  DevName = String()
get_device(DevName) ->
   gen_server:call(?db_ref, {get_device, DevName}).

% update_device(Device()) -> Device() | error
update_device(Device) ->
   gen_server:call(?db_ref, {update_device, Device}).

% register() -> Device() | error
%  registers a new device
register() ->
   gen_server:call(?db_ref, register).

% reister(DevName) -> Device() | no_such_device | already_registered | error
%  registers a existing device
register(DevName) ->
   gen_server:call(?db_ref, {register, DevName}).

% unregister(DevName) -> unregistered | no_such_device | error | wrong_owner
unregister(DevName) ->
   gen_server:call(?db_ref, {unregister, DevName}).

% new_user(UserName, Password) -> User | already_exists | error
%  UserName = String(), e.g. demo@demo.org
%  Password = String()
%  User = #user, see common_defs.hrl for details
new_user(UserName, Password) ->
   gen_server:call(?db_ref, {new_user, UserName, Password}).

% update_user(User()) -> User() | no_such_user | error
update_user(User) ->
   gen_server:call(?db_ref, {update_user, User}).

% get_user(UserName) -> User()
%  UserName = String()
get_user(UserName) ->
   gen_server:call(?db_ref, {get_user, UserName}).

% authenticate(UserName, Password) -> User() | rejected | error
%  UserName = String()
%  Password = String()
authenticate(UserName, Password) ->
   gen_server:call(?db_ref, {login, UserName, Password}).

% logout(UserName) -> ok | rejected | error
%  UserName = String()
%logout(UserName) ->
%   gen_server:call(?db_ref, {logout, UserName}).

% get_tracks(DevName) -> Tracks | error
%  Tracks = [Track]
%  Track = #track. See common_defs.hrl for details
get_tracks(DevName) ->
   gen_server:call(?db_ref, {get_tracks, DevName}).

% get_triggers(DevName) -> Triggers | error
%  Triggers = [Trigger]
%  Trigger = #trigger. See common_defs.hrl for details
get_triggers(DevName) ->
   gen_server:call(?db_ref, {get_triggers, DevName}).

get_news() ->
   gen_server:call(?db_ref, {get_news, undef}).

get_news(UpToDate) ->
   gen_server:call(?db_ref, {get_news, UpToDate}).

insert_news(Date, Text) ->
   gen_server:call(?db_ref, {insert_news, Date, Text}).

delete_news(NewsRef) ->
   gen_server:call(?db_ref, {delete_news, NewsRef}).

new_track(DevName, Force) ->
   Track = gen_server:call(?db_ref, {new_track, DevName, Force, []}),
   TrackPid = gtracker_track:start(Track, ?db_ref),
   Device = get_device(DevName),
   Links = Device#device.links,
   update_device(Device#device{links = Links#links{track = TrackPid}}),
   TrackPid.
