-module(user_settings).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").

main() ->
   #template { file="./site/templates/index.html" }.

title() ->
   "GTracker - User Settings".

body() ->
   display(wf:user()).

display(undefined) ->
   #h1 { text="Please login first before change settings" };

display(_User) ->
   #panel { body=["settings dialog"] }.
