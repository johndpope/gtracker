-module(contacts).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").

title() ->
   "Contacts".

main() ->
   #template { file="./site/templates/index.html" }.

render() ->
   #template { file="./site/templates/pages/contacts" }.
