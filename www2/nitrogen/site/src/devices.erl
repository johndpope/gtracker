-module(devices).

-include_lib("nitrogen/include/wf.hrl").
-include("db.hrl").

%-export([display/0]).
-compile(export_all).

display() ->
   display(wf:user()).

display(undefined) ->
   [];

display(_User) ->
   [
      change_map(),
      display_devices(wf:session(devices))
   ].

display_devices(List) ->
   lists:map(fun display_device/1, List).

change_map() ->
   wf:f("$map.setBaseLayer($map.layers[~s]);", [wf:session(map_id)]).

display_device([DeviceID]) ->
   case q:exec(?DEVICE_LAYER_SETTIGS, [DeviceID]) of
      ?RESULT([[Color, Weight, Name, Alias]]) ->
         {ok, Pid} = wf:comet(fun() -> loop(DeviceID) end, DeviceID),
         gen_server:cast({global, fake_server}, {subscribe, DeviceID, Pid}),
         wf:f("device_create(~w, { color:'~s', width:~w, name:'~s', alias:'~s' });", [DeviceID, Color, Weight, Name, Alias]);
      _ ->
         wf:f(" /* Bad Device: ~w */ ", [DeviceID])
   end.

loop(DeviceID) ->
   receive
      online ->
         wf:wire(wf:f("device_change_state(~w, 'online');", [DeviceID])),
         wf:flush();

      offline ->
         wf:wire(wf:f("device_change_state(~w, 'offline');", [DeviceID])),
         wf:flush();
      
      {coord, Lat, Lon, _Speed, _Timestamp} ->
         wf:wire(wf:f("device_move(~w, ~f, ~f);", [DeviceID, Lon, Lat])),
         wf:flush()
   end,
   loop(DeviceID).
