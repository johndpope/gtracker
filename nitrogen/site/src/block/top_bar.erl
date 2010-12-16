-module(top_bar).
-include_lib("nitrogen/include/wf.hrl").
-export([render/0, event/1]).

render() ->
   #panel { class=wrapper, body=
      #panel { body=[
            case wf:user() of
               undefined ->
                  render_login();
               _ ->
                  render_logout()
            end
         ]}
   }.

render_logout() ->
   IsAdmin = user:admin(),
   [
      #panel { class="info", body=[
            #gravatar { email=wf:user(), size="22", default="mm" },
            #span { text=wf:user() },
            #span { text=" (admin)", class=admin, show_if=IsAdmin },
            #panel { class=control, body=[
                  #link { text="Settings", url="/user_settings" },
                  #link { text="News Editor", show_if=IsAdmin, url="/news_editor" }
               ]}
         ]},
      #link { class=logout, text="Logout", postback=logout, delegate=?MODULE }
   ].

render_login() ->
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
   Username = wf:q(user_text_box),
   Password = wf:q(password_text_box),
   case gtracker_db_pub:authenticate(Username, Password) of
      rejected ->
         client:warning("email/password combination is mismatch");
      error ->
         client:error("unknown");
      UserInfo ->
         user:save_into_session(UserInfo),
         wf:redirect(wf:path_info())
   end.
