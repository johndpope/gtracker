-module(map).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include("records.hrl").

main() ->
   #template { file="./site/templates/index.html" }.

title() ->
   "GTracker".

body() ->
   #map { id=map, style="height: 480px; border: 1px solid black;" }.
