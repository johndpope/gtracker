-module(contacts).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").

main() ->
   #template { file="./site/templates/index.html" }.

title() ->
   "GTracker - Contacts".

body() ->
   #template { file="./site/templates/contacts_body.html" }.
