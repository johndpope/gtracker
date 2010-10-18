-module (element_map).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include("records.hrl").

reflect() -> record_info(fields, map).
   
render_element(R = #map{}) ->
   Body = #panel{ id=R#map.id, style=R#map.style, class=R#map.class },
   wf:wire(wf:f("createMap(obj('me'), ~f, ~f, ~w);",
         [ R#map.lon, R#map.lat, R#map.zoom ])),
   Body.
