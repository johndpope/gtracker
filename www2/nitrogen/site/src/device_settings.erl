-module(device_settings).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include_lib("gtracker_db_pub/include/common_recs.hrl").

main() ->
   #template { file="./site/templates/index.html" }.

title() ->
   "Device Settings".

render() ->
   #panel { class=?MODULE, body=display(wf:session(device_to_edit)) }.

bool(true) ->
   "Yes";
bool(false) ->
   "No".

display(undefined) ->
   #h1 { text="Device not selected" };

display(DeviceName) ->
   case gtracker_db_pub:get_device(DeviceName) of
      error ->
         #template { file="./site/templates/messages/bad_device" };
      Device ->
         display_device(Device)
   end.

display_device(Device = #device{name=Name, online=Online, alias=Alias, timezone=Timezone, color=Color, weight=Weight}) ->
   [
      #panel { class=view, body=[
            #h2 { text="Information:" },
            #table { rows=[
                  #tablerow { cells=[
                        #tablecell { class=first, body="Name:" },
                        #tablecell { body=Name }
                     ]},
                  #tablerow { cells=[
                        #tablecell { class=first, body="Online:" },
                        #tablecell { body=bool(Online) }
                     ]}
               ]}
         ]},
      #panel { class=view, body=[
            #h2 { text="Edit look and feel:" },
            #table { rows=[
                  #tablerow { cells=[
                        #tablecell { class=first, body="Alias:" },
                        #tablecell { body=#textbox { id=alias, text=Alias } }
                     ]},
                  #tablerow { cells=[
                        #tablecell { class=first, body="Color:" },
                        #tablecell { body=#textbox { id=color, text=Color } }
                     ]},
                  #tablerow { cells=[
                        #tablecell { class=first, body="Line Width:" },
                        #tablecell { body=#textbox { id=weight, text=integer_to_list(Weight) } }
                     ]},
                  #tablerow { cells=[
                        #tablecell { class=first, body="Timezone:" },
                        #tablecell { body=#textbox { id=timezone, text=Timezone } }
                     ]}
               ]},
            #button { text="Save", postback={save, Device} },
            #button { text="Cancel", postback=cancel }
         ]}
   ].

event({save, Device}) ->
   NewDevice = Device#device{ alias=wf:q(alias), color=wf:q(color), weight=list_to_integer(wf:q(weight)),
      timezone=wf:q(timezone) },
   case device:update(NewDevice) of 
      error ->
         client:error("Failed to save settings");
      _UpdatedDevice ->
         wf:session(device_to_edit, undefined),
         wf:redirect("/user_settings") % this page is iniciator of device_settings
   end;

event(cancel) ->
   wf:session(device_to_edit, undefined),
   wf:redirect("/user_settings"). % this page is iniciator of device_settings
