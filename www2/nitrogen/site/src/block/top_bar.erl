-module(top_bar).
-include_lib("nitrogen/include/wf.hrl").
-export([display/0, event/1]).
-include("db.hrl").

display() ->
   #panel { class=wrapper, body=
      #panel { body=[
            case wf:user() of
               undefined ->
                  display_login();
               _ ->
                  display_logout()
            end
         ]}
   }.

display_logout() ->
   [
      #panel { class="info", body=[
            #gravatar { email=wf:user(), size="22" },
            #span { text=wf:user() },
            #span { text=" (admin)", class=admin, show_if=wf:session(admin) },
            #panel { class=control, body=[
                  #link { text="Settings", url="/user_settings" },
                  #link { text="News Editor", show_if=wf:session(admin), url="/news_editor" }
               ]}
         ]},
      #link { class=logout, text="Logout", postback=logout, delegate=?MODULE }
   ].

display_login() ->
   [
      #span { text="Email: " },
      #textbox { id=user_text_box, next=password_text_box },
      #span { text=" Password: " },
      #password { id=password_text_box, postback=login, delegate=?MODULE },
      #link { id=login_link, text="Login", postback=login, delegate=?MODULE },
      #link { class=registration, text="Registration", url="/registration" }
   ].

event(logout) ->
   wf:clear_session(),
   wf:redirect(wf:path_info());

event(login) ->
   User = wf:q(user_text_box),
   Password = erlang:list_to_binary(md5:hex(wf:q(password_text_box))),
   case q:exec(?USER_AUTHENTICATE, [User]) of
      ?RESULT([[UserID,RealPassword]]) when RealPassword == Password ->
         wf:user(User),
         user:id(UserID),
         user:load_settings_into_session(UserID),
         user:load_devices_into_session(UserID),
         wf:redirect(wf:path_info());

      ?RESULT([[_,_]]) ->
         wf:wire(#alert { text="Incorrect password" });

      ?RESULT([]) ->
         wf:wire(#alert { text="Account not exists" });

      bad_query ->
         wf:wire(#alert { text="INTERNAL ERROR" })
   end.
