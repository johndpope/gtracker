-module(device_settings).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include("db.hrl").

main() ->
   #template { file="./site/templates/index.html" }.

title() ->
   "GTracker - Device Settings".

body() ->
   #panel { class=?MODULE, body=display(wf:session(device_id_settings)) }.

display(undefined) ->
   #h1 { text="Device not selected" };

display(DeviceID) ->
   ?RESULT([[Color, Width, Name, Alias, TwitterAuth]]) = q:exec(?DEVICE_SETTINGS, [DeviceID]),
   #panel { class=view, body=[
         #h2 { id=name, text=Name },
         #br {},
         #table { rows=[
               #tablerow { cells=[
                     #tablecell { body="Alias:" },
                     #tablecell { body=#textbox { id=alias, text=Alias } }
                  ]},
               #tablerow { cells=[
                     #tablecell { body="Color:" },
                     #tablecell { body=#textbox { id=color, text=Color } }
                  ]},
               #tablerow { cells=[
                     #tablecell { body="Line width:" },
                     #tablecell { body=#textbox { id=width, text=integer_to_list(Width) } }
                  ]},
               #tablerow { cells=[
                     #tablecell { body="Twitter link:" },
                     #tablecell { body=twitter_auth(TwitterAuth) }
                  ]}
            ]},
         #br {},
         #button { text="Save", postback={save, DeviceID} },
         #button { text="Cancel", postback=cancel }
      ]}.

twitter_auth(undefined) ->
   #link { text="Make connection", url="link_twitter" };

twitter_auth(_Ok) ->
   "Twitter-ready".

event({save, DeviceID}) ->
   Alias=wf:q(alias),
   Color=wf:q(color),
   Width=wf:q(width),
   q:exec(?DEVICE_UPDATE_INFO, [Color, Width, Alias, DeviceID]),
   wf:session(device_id_settings, undefined),
   wf:redirect("/user_settings"); % this page is iniciator of device_settings

event(cancel) ->
   wf:session(device_id_settings, undefined),
   wf:redirect("/user_settings"). % this page is iniciator of device_settings
