-module(map).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include("records.hrl").
-define(MAP_STYLE, "height: 480px; border: 1px solid black;").

main() ->
   #template { file="./site/templates/index.html" }.

script() ->
   #template { file="./site/templates/map_script.html" }.

title() ->
   "GTracker".

body() ->
   #map { id=map, style=?MAP_STYLE }.
