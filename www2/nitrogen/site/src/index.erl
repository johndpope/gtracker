-module(index).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").

main() ->
   #template { file="./site/templates/index.html" }.

title() ->
   "GTracker - Wellcome".

body() ->
   #template { file="./site/templates/welcome_body.html" }.