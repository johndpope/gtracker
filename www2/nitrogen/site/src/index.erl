-module(index).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include("records.hrl").

main() ->
   #template { file="./site/templates/index.html" }.

title() ->
   "GTracker".

body() ->
   #panel { id="shit", body=["Hi friend, it's a welcome page"] }.
%   #map { id=map, style="height: 480px; border: 1px solid black;" }.
