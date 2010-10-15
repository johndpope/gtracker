-module (element_map).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include("records.hrl").

reflect() -> record_info(fields, map).

render_element(_Record = #map{}) ->
   LoadJS = "$.getScript('http://maps.google.com/maps/api/js?sensor=false', function() { $.getScript('/gtracker/test.js'); });",
   wf:wire(LoadJS),
   #panel { id=map }.
