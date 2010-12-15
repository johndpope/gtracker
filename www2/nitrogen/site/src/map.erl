-module(map).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").

main() ->
   #template { file="./site/templates/index.html" }.

script() ->
   #template { file="./site/templates/scripts/map" }.

title() ->
   "Map".

render() ->
   #template { file="./site/templates/pages/map" }.
