-module(map).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include("db.hrl").

main() ->
   #template { file="./site/templates/index.html" }.

script() ->
   #template { file="./site/templates/map_script.html" }.

title() ->
   "GTracker - Map".

body() ->
   wf:wire(#api { name=device, tag=show }),
   [
      #template { file="./site/templates/map_body.html" },
      "<a onclick='page.device(427)'>create</a>",
      #panel { id=devices },
      #panel { style="clear: both" }
   ].

api_event(device, show, [Id]) ->
   Body = case q:exec(?SHOW_DEVICE_INFO, [Id]) of
      ?RESULT([[Name,Alias,Online]]) ->
            wf:f("<div id='d~w' class='device'><div>Name: ~s</div><div>Alias: ~s</div><div>Online: ~w</div></div>", [Id, Name, Alias, Online]);
      ?RESULT([]) ->
         "Bad"
   end,
   wf:insert_bottom(devices, Body).
