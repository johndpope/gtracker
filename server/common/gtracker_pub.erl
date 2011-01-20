-module(gtracker_pub).

-compile({no_auto_import,[register/2]}).

-export(
   [
      get_devices/0, get_devices/2
      ,get_device/1, get_device/3
      ,update/2, update/4
      ,register/0, register/1, register/2, register/3
      ,unregister/1, unregister/3
      ,subscribe/1, subscribe/3
      ,unsubscribe/1, unsubscribe/3
      ,new_user/2, new_user/4
      ,get_user/1, get_user/3
      ,authenticate/2, authenticate/4
      ,get_news/0, get_news/2
      ,get_news/1, get_news/3
      ,insert_news/2, insert_news/4
      ,delete_news/1, delete_news/3
      ,get_tracks/1, get_tracks/3
      ,new_track/3, new_track/5
      ,get_triggers/1, get_triggers/3
      ,get_current_track/1, get_current_track/3
   ]).

-include("common_recs.hrl").
-include("common_defs.hrl").


% get_devices(Db, Timeout) -> [Device] | {error, term(), List}
%  Db = registereg Db name
%  Timeout - call timeout
%  Device = #device, See device record in gtracker/server/include/common_defs.hrl for details
get_devices(Db, Timeout) ->
   call(Db, get_all_devices, Timeout).
get_devices() ->
   get_devices(?db_ref, ?MAX_CALL_TIMEOUT).

% get_device(Db, DevName, Timeout) -> Device() | {error, term(), List}
%  Db = registereg Db name
%  Timeout - call timeout
%  DevName = String()
get_device(Db, DevName, Timeout) ->
   call(Db, {get_device, DevName}, Timeout).
get_device(DevName) ->
   get_device(?db_ref, DevName, ?MAX_CALL_TIMEOUT).

% update(Db, Object(), Mask, Timeout) -> Object() | {error, term(), List}
%  Db = registereg Db name
%  Object = Device(), Track(), User()
%  Timeout - call timeout
%  Mask = [FieldName]
%  FieldName = atom()
update(Db, Object, Mask, Timeout) ->
   call(Db, {update, Object, Mask}, Timeout).
update(Object, Mask) ->
   update(?db_ref, Object, Mask, ?MAX_CALL_TIMEOUT).

% register(Db, Timeout) -> Device() | {error, term(), List}
%  Db = registereg Db name
%  Timeout - call timeout
%  registers a new device
register(Db, Timeout) ->
   call(Db, register, Timeout).
register() ->
   register(?db_ref, ?MAX_CALL_TIMEOUT).

% reister(Db, DevName, Timeout) -> Device() | {error, term(), List}
%  Db = registereg Db name
%  Timeout - call timeout
%  registers a existing device
register(Db, DevName, Timeout) ->
   call(Db, {register, DevName}, Timeout).
register(DevName) ->
   register(?db_ref, DevName, ?MAX_CALL_TIMEOUT).

% unregister(Db, DevName, Timeout) -> unregistered | {error, term(), List}
%  Db = registereg Db name
%  Timeout - call timeout
unregister(Db, DevName, Timeout) ->
   call(Db, {unregister, DevName}, Timeout).
unregister(DevName) ->
   unregister(?db_ref, DevName, ?MAX_CALL_TIMEOUT).

% subscribe(Db, DevName, Timeout) -> ok | atom()
%  Db = registereg Db name
%  Timeout - call timeout
%  DevName = Sttring()
subscribe(Db, DevName, Timeout) ->
   case  call(Db, {subscribe, DevName, self()}, Timeout) of
      {ok, Device = #device{owner = undef}} ->
         {ok, Device};
      {ok, Device = #device{owner = Owner}} ->
         Owner ! {updated, Device},
         {ok, Device};
      Err ->
         Err
   end.
subscribe(DevName) ->
   subscribe(?db_ref, DevName, ?MAX_CALL_TIMEOUT).

% unsubscribe(Db, DevName, Timeout) -> ok | atom()
%  Db = registereg Db name
%  Timeout - call timeout
%  DevName = String()
unsubscribe(Db, DevName, Timeout) ->
   case  call(Db, {unsubscribe, DevName, self()}, Timeout) of
      {ok, Device = #device{owner = undef}} ->
         {ok, Device};
      {ok, Device = #device{owner = Owner}} ->
         Owner ! {updated, Device},
         {ok, Device};
      Err ->
         Err
   end.
unsubscribe(DevName) ->
   unsubscribe(?db_ref, DevName, ?MAX_CALL_TIMEOUT).

% new_user(Db, UserName, Password, Timeout) -> User | {error, term(), List}
%  Db = registereg Db name
%  Timeout - call timeout
%  UserName = String(), e.g. demo@demo.org
%  Password = String()
%  User = #user, see common_defs.hrl for details
new_user(Db, UserName, Password, Timeout) ->
   call(Db, {new_user, UserName, Password}, Timeout).
new_user(UserName, Password) ->
   new_user(?db_ref, UserName, Password, ?MAX_CALL_TIMEOUT).

% get_user(Db, UserName, Timeout) -> User()
%  Db = registereg Db name
%  Timeout - call timeout
%  UserName = String()
get_user(Db, UserName, Timeout) ->
   call(Db, {get_user, UserName}, Timeout).
get_user(UserName) ->
   get_user(?db_ref, UserName, ?MAX_CALL_TIMEOUT).

% authenticate(Db, UserName, Password, Timeout) -> User() | {error, term(), List}
%  Db = registereg Db name
%  Timeout - call timeout
%  UserName = String()
%  Password = String()
authenticate(Db, UserName, Password, Timeout) ->
   call(Db, {login, UserName, Password}, Timeout).
authenticate(UserName, Password) ->
   authenticate(?db_ref, UserName, Password, ?MAX_CALL_TIMEOUT).

% get_tracks(Db, DevName, Timeout) -> Tracks | {error, term(), List()}
%  Db = registereg Db name
%  Timeout - call timeout
%  Tracks = [Track]
%  Track = #track. See common_defs.hrl for details
get_tracks(Db, DevName, Timeout) ->
   call(Db, {get_tracks, DevName}, Timeout).
get_tracks(DevName) ->
   get_tracks(?db_ref, DevName, ?MAX_CALL_TIMEOUT).

% get_triggers(Db, DevName, Timeout) -> Triggers | {error, term(), List()}
%  Db = registereg Db name
%  Timeout - call timeout
%  Triggers = [Trigger]
%  Trigger = #trigger. See common_defs.hrl for details
get_triggers(Db, DevName, Timeout) ->
   call(Db, {get_triggers, DevName}, Timeout).
get_triggers(DevName) ->
   get_triggers(?db_ref, DevName, ?MAX_CALL_TIMEOUT).

get_news(Db, Timeout) ->
   call(Db, {get_news, undef}, Timeout).
get_news() ->
   get_news(?db_ref, ?MAX_CALL_TIMEOUT).

get_news(Db, UpToDate, Timeout) ->
   call(Db, {get_news, UpToDate}, Timeout).
get_news(UpToDate) ->
   get_news(?db_ref, UpToDate, ?MAX_CALL_TIMEOUT).

insert_news(Db, Date, Text, Timeout) ->
   call(Db, {insert_news, Date, Text}, Timeout).
insert_news(Date, Text) ->
   insert_news(?db_ref, Date, Text, ?MAX_CALL_TIMEOUT).

delete_news(Db, NewsRef, Timeout) ->
   call(Db, {delete_news, NewsRef}, Timeout).
delete_news(NewsRef) ->
   delete_news(?db_ref, NewsRef, ?MAX_CALL_TIMEOUT).

new_track(Db, DevName, Force, CalcSpeed, Timeout) ->
   case call(Db, {new_track, DevName, Force}, Timeout) of
      {ok, Track} ->
         case call(Track#track.track_server, {new_track, Track#track.id, CalcSpeed}, Timeout) of
            {ok, _} ->
               {ok, Track};
            Err ->
               Err
         end;
      Err ->
         Err
   end.
new_track(Device, Force, CalcSpeed) ->
   new_track(?db_ref, Device, Force, CalcSpeed, ?MAX_CALL_TIMEOUT).

get_current_track(Db, DevName, Timeout) ->
   call(Db, {get_current_track, DevName}, Timeout).
get_current_track(DevName) ->
   get_current_track(?db_ref, DevName, ?MAX_CALL_TIMEOUT).

call(ServerRef, Request, Timeout) ->
   case (catch gen_server:call(ServerRef, Request, Timeout)) of
      {'EXIT', {Err, Req}} ->
         {error, Err, [Req]};
      Other ->
         Other
   end.
