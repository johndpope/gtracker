-module(index).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").

main() ->
   #template { file="./site/templates/index.html" }.

title() ->
   "Welcome".

render() ->
   #template { file="./site/templates/pages/welcome" }.
