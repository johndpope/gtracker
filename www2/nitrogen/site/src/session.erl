-module(session).

-include_lib("nitrogen/include/wf.hrl").
-include("db.hrl").

-export([display/0, event/1]).

display() ->
   #panel { class=wrapper, body=
      #panel { id=session, body=[
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
      #panel { class="user", body=[
            #gravatar { email=wf:user(), size="22" },
            #span { text=wf:user() }
         ]},
      #link { class=logout, text="Logout", postback=logout, delegate=?MODULE }
   ].

display_login() ->
   [
      #span { text="Email: " },
      #textbox { id=user_text_box, class=form, next=password_text_box },
      #span { text=" Password: " },
      #password { id=password_text_box, class=form, postback=login, delegate=?MODULE },
      #link { id=login_link, text="Login", postback=login, delegate=?MODULE },
      #link { class=registration, text="Registration", url="/registration" }
   ].

event(logout) ->
   wf:clear_session(),
   wf:redirect(wf:path_info());

event(login) ->
   User = wf:q(user_text_box),
   Password = erlang:list_to_binary(md5:hex(wf:q(password_text_box))),
   case q:exec(?AUTHENTICATE, [User]) of
      ?RESULT([[_Id, RealPassword]]) when RealPassword == Password ->
         wf:user(User),
         wf:redirect(wf:path_info());
      ?RESULT([[_, _]]) ->
         wf:wire(#alert { text="Incorrect password" });
      ?RESULT([]) ->
         wf:wire(#alert { text="Account not exists" });
      bad_query ->
         wf:wire(#alert { text="INTERNAL ERROR" })
   end.
