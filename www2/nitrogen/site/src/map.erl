-module(map).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").

main() ->
   #template { file="./site/templates/index.html" }.

script() ->
   #template { file="./site/templates/map_script.html" }.

title() ->
   "GTracker - Map".

body() ->
   #template { file="./site/templates/map_body.html" }.
