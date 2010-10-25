-module(registration).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include("records.hrl").
-include("db.hrl").

main() -> #template { file="./site/templates/registration.html" }.

title() -> "GTracker - Registration".

body() ->
   wf:wire(register_button, user_text_box, #validate {
         validators=[
            #custom { text="Email address already used", function=fun check_user/2 },
            #is_email { text="Not a valid email address" }
         ]}),
   wf:wire(register_button, password_text_box, #validate {
         validators=[
            #min_length { text="Minimum of 6 characters", length=6 }
         ]}),
   wf:wire(register_button, password_confirm_text_box, #validate {
         validators=[
            #confirm_password { text="Password must match", password=password_text_box }
         ]}),
   [
      #panel { class=registration_form, body=[
            #label { text="Your email address:" },
            #textbox { id=user_text_box, next=password_text_box },
            #label { text="Your password:" },
            #password { id=password_text_box, next=password_confirm_text_box },
            #label { text="Confirm password:" },
            #password { id=password_confirm_text_box, next=register_button },
            #p {},
            #button { id=register_button, text="Register", postback=register_user }
         ]},
      #hr {},
      #panel { body=[
            "You can set own uniq avatar on the site ", #link { text="gravatar", url="http://en.gravatar.com/" }
         ]}
   ].

check_user(_Tag, User) ->
   case q:exec(?USER_EXISTS, [User]) of
      ?RESULT([[_Id]]) ->
         false;
      ?RESULT([]) ->
         true
   end.

event(register_user) ->
   User = wf:q(user_text_box),
   Password = erlang:list_to_binary(md5:hex(wf:q(password_text_box))),
   q:exec(?USER_CREATE, [User, Password]),
   wf:user(User),
   wf:redirect("/").
