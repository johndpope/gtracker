-module(devices).
-include_lib("nitrogen/include/wf.hrl").
-include_lib("gtracker_db_pub/include/common_recs.hrl").
%-export([display/0]).
-compile(export_all).

-define(DEVICE_CHANGE_STATE, "device_change_state(~w, '~s');").
-define(DEVICE_MOVE, "device_move(~w, ~f, ~f);").

display() ->
   display(wf:user()).

display(undefined) ->
   [];

display(_User) ->
   [
      change_map(),
      display_devices(user:device_list())
   ].

display_devices(List) ->
   lists:map(fun display_device/1, List).

change_map() ->
   wf:f("$map.setBaseLayer($map.layers[~w]);", [user:map_type()]).

display_device({Name, error}) ->
   wf:f(" /* Bad Device: ~w */ ", [Name]);

display_device(_Device = #device{name=Name, color=Color, weight=Weight, alias=Alias}) ->
   wf:f("device_create(~w, { color:'~s', width:~w, name:'~s', alias:'~s' });", [0, Color, Weight, Name, Alias]).
%   case q:exec(?DEVICE_LAYER_SETTIGS, [DeviceID]) of
%      ?RESULT([[Color, Weight, Name, Alias]]) ->
%         {ok, Pid} = wf:comet(fun() -> loop(DeviceID) end, DeviceID),
%         gen_server:cast({global, fake_server}, {subscribe, DeviceID, Pid}),
%         wf:f("device_create(~w, { color:'~s', width:~w, name:'~s', alias:'~s' });", [DeviceID, Color, Weight, Name, Alias]);
%      _ ->
%         wf:f(" /* Bad Device: ~w */ ", [DeviceID])
%   end.

loop(DeviceID) ->
   receive
      online ->
         w:exec(?DEVICE_CHANGE_STATE, [DeviceID, "online"]),
         wf:flush();

      offline ->
         w:exec(?DEVICE_CHANGE_STATE, [DeviceID, "offline"]),
         wf:flush();
      
      {coord, Lat, Lon, _Speed, _Timestamp} ->
         w:exec(?DEVICE_MOVE, [DeviceID, Lon, Lat]),
         wf:flush()
   end,
   loop(DeviceID).
