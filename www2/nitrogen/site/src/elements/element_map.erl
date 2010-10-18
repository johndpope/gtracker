-module (element_map).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include("records.hrl").

reflect() -> record_info(fields, map).
   
render_element(R = #map{}) ->
   %wf:wire(wf:f("$(~p).css('background', 'red');", [ R#map.id ])),
   MapId = wf:temp_id(),
   Body = #panel{ id=R#map.id, body=[
         wf:f("<div id=~p style=~p class=~p></div>", [MapId, R#map.style, R#map.class])
      ]},

   wf:wire(wf:f("var _map = new Map.Map(~p); _map.setCenter(~f,~f); _map.setZoom(~w); $~w = _map;",
         [ MapId, R#map.lon, R#map.lat, R#map.zoom, MapId ])),

   Body.
