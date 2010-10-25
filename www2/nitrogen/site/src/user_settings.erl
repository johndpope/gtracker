-module(user_settings).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include("db.hrl").

main() ->
   #template { file="./site/templates/index.html" }.

title() ->
   "GTracker - User Settings".

body() ->
   #panel { class=?MODULE, body=display(user:id()) }.

display(undefined) ->
   #h1 { text="Please login first before change settings" };

display(UserID) ->
   ?RESULT(Devices) = q:exec(?DEVICE_NAME_LIST, [UserID]),
   wf:wire(add_button, device_text_box, #validate {
         validators=[
            #custom { text="Device not exists", function=fun device_exists_validator/2 }
         ]}),
   [
      #panel { class=view, body=[
            #h2 { text="Your devices:" },
            #table { rows=[
                  #bind { data=Devices, map=[ name@text, settings@postback, remove@postback ], transform=fun convert/2,
                     body=#tablerow { cells=[
                           #tablecell { class=first, id=name },
                           #tablecell { body=#link { id=settings, text="Settings" } },
                           #tablecell { body=#link { id=remove, text="Remove" } }
                        ]}
                  }
               ]}
         ]},
      #panel { class=view, body=[
            #h2 { text="Add device:" },
            #textbox { id=device_text_box, postback=add },
            #p {},
            #button { id=add_button, text="Add", postback=add }
         ]},
      #panel { class=view, body=[
            #h2 { text="Look and feel:" },
            #span { text="Default Map:" },
            #dropdown { id=map_type_dropdown, value=wf:session_default(map_id, "0"), options=[
                  #option { text="OSM Mapnik", value="0" },
                  #option { text="OSM CycleMap", value="1" },
                  #option { text="OSM Osmarender", value="2" },
                  #option { text="Google Maps", value="3" },
                  #option { text="Yandex Maps", value="4" }
               ]},
            #p {},
            #button { text="Change", postback=change }
         ]}
   ].

convert(DataRow, Acc) ->
   [DeviceID, Name] = DataRow,
   { [Name, {settings, DeviceID}, {remove, user:id(), DeviceID}], Acc, [] }.

device_exists_validator(_Tag, Value) ->
   case q:exec(?DEVICE_EXISTS, [Value]) of
      ?RESULT([[_Id]]) ->
         true;
      ?RESULT([]) ->
         false
   end.

event({settings, DeviceID}) ->
   wf:session(device_id_settings, DeviceID),
   wf:redirect("/device_settings");

event({remove, UserID, DeviceID}) ->
   q:exec(?USER_REMOVE_DEVICE, [UserID, DeviceID]),
   user:load_devices_into_session(),
   wf:redirect(wf:path_info());

event(add) ->
   ?RESULT([[DeviceID]]) = q:exec(?DEVICE_EXISTS, [wf:q(device_text_box)]),
   q:exec(?USER_ADD_DEVICE, [user:id(), DeviceID]),
   user:load_devices_into_session(),
   wf:redirect(wf:path_info());

event(change) ->
   MapType=wf:q(map_type_dropdown),
   wf:session(map_id, MapType),
   q:exec(?USER_SAVE_SETTINGS, [MapType, user:id()]),
   wf:redirect(wf:path_info()).
