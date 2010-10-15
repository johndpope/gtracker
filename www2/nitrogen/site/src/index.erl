-module(index).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include("records.hrl").

main() -> #template { file="./site/templates/root.html" }.

title() -> "GTracker beta".

body() ->
   #map {}.
