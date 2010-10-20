-module(devices).

-include_lib("nitrogen/include/wf.hrl").
-include("db.hrl").

-export([display/0]).

display() ->
   display_user_devices(wf:user()).

display_user_devices(undefined) ->
   [];

display_user_devices(_User) ->
   display_devices(wf:session(devices)).

display_devices(undefined) ->
   % query devices list and store they in session
   Items = [ 426, 427, 456, 499 ],
   wf:session(devices, Items),
   display_devices(Items);

display_devices(List) ->
   lists:map(fun display_device/1, List).

display_device(DeviceId) ->
   case q:exec(?DEVICE_INFO, [DeviceId]) of
      ?RESULT([[Color, Weight, Name, Alias]]) ->
         wf:f("createDevice(~w, { color:'~s', width:~w, name:'~s', alias:'~s' });", [DeviceId, Color, Weight, Name, Alias]);
      _ ->
         wf:f(" /* Bad Device: ~w */ ", [DeviceId])
   end.
