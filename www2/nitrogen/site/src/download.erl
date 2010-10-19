-module(download).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").

main() ->
   #template { file="./site/templates/index.html" }.

title() ->
   "GTracker - Download".

body() ->
   #template { file="./site/templates/download_body.html" }.
