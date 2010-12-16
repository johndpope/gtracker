-module(user_settings).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include_lib("gtracker_db_pub/include/common_recs.hrl").

main() ->
   #template { file="./site/templates/index.html" }.

title() ->
   "User Settings".

render() ->
   #panel { class=?MODULE, body=display(wf:user()) }.

display(undefined) ->
   #template { file="./site/templates/messages/logon_first" };

display(Username) ->
   [
      #panel { class=view, body=[
            #h2 { text="Information:" },
            #table { rows=[
                  #tablerow { cells=[
                        #tablecell { class=first, body="Username:" },
                        #tablecell { body=Username }
                     ]},
                  #tablerow { cells=[
                        #tablecell { class=first, body="Avatar:" },
                        #tablecell { body=[
                              "<div align=\"center\">",
                              #link { body=#gravatar { email=Username, size="64", default="mm" }, url="http://gravatar.com" },
                              "<div>"
                           ]}
                     ]}
               ]}
         ]},
      #panel { class=view, body=[
            #h2 { text="Your devices:" },
            #table { rows=[
                  #bind { data=user:devices(), map=[ name@text, settings@postback, remove@postback ], transform=fun convert/2,
                     body=#tablerow { cells=[
                           #tablecell { class=first, id=name },
                           #tablecell { body=#link { id=settings, text="Settings" } },
                           #tablecell { body=#link { id=remove, text="Remove" } }
                        ]}
                  }
               ]},
            #br {},
            #h2 { text="Add device:" },
            #textbox { id=device_text_box, postback=add },
            #button { id=add_button, text="Add", postback=add }
         ]},
      #panel { class=view, body=[
            #h2 { text="Look and feel:" },
            #span { text="Default Map:" },
            #dropdown { id=map_type_dropdown, value=user:map_type(), options=[
                  #option { text="OSM Mapnik", value=0 },
                  #option { text="OSM CycleMap", value=1 },
                  #option { text="OSM Osmarender", value=2 },
                  #option { text="Google Maps", value=3 }
               ]},
            #p {},
            #button { text="Save", postback={save,wf:session(user_info)} }
         ]}
   ].

convert(DeviceName, Acc) ->
   { [DeviceName, {settings, DeviceName}, {remove, wf:session(user_info), DeviceName}], Acc, [] }.

event({settings, DeviceName}) ->
   wf:session(device_to_edit, DeviceName),
   wf:redirect("/device_settings");

event({remove, UserInfo, DeviceName}) ->
   NewUserInfo = UserInfo#user{ devices=lists:delete(DeviceName, UserInfo#user.devices) },
   case user:update(NewUserInfo) of
      error ->
         client:error("Failed to update");
      UpdatedUserInfo ->
         user:save_into_session(UpdatedUserInfo),
         wf:redirect(wf:path_info())
   end;

event(add) ->
   case gtracker_db_pub:get_device(wf:q(device_text_box)) of
      no_such_device ->
         client:error("device not exists");
      error ->
         client:error("unknown error");
      Device ->
         DeviceName = Device#device.name,
         UserInfo = wf:session(user_info),
         NewUserInfo = UserInfo#user{ devices=[DeviceName | UserInfo#user.devices] },
         case user:update(NewUserInfo) of
            error ->
               client:error("Failed to update");
            UpdatedUserInfo ->
               user:save_into_session(UpdatedUserInfo),
               wf:redirect(wf:path_info())
         end
   end;

event({save, UserInfo}) ->
   NewMapType = wf:q(map_type_dropdown),
   NewUserInfo = UserInfo#user{map_type=NewMapType},
   case user:update(NewUserInfo) of
      error ->
         client:error("Failed to update");
      UpdatedUserInfo ->
         user:save_into_session(UpdatedUserInfo),
         wf:redirect(wf:path_info())
   end.
