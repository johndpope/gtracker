-module(registration).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").

main() -> #template { file="./site/templates/registration.html" }.

render() ->
   wf:wire(register_button, user_text_box, #validate {
         validators=[
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
            #panel { body="Your email address:" },
            #textbox { id=user_text_box, next=password_text_box },
            #panel { body="Your password:" },
            #password { id=password_text_box, next=password_confirm_text_box },
            #panel { body="Confirm password:" },
            #password { id=password_confirm_text_box, next=register_button },
            #p {},
            #button { id=register_button, text="Register", postback=register_user }
         ]},
      #hr {},
      #panel { body=[
            "You can set own uniq avatar on the site ", #link { text="gravatar", url="http://en.gravatar.com/" }
         ]}
   ].

event(register_user) ->
   Username = wf:q(user_text_box),
   Password = wf:q(password_text_box),
   case gtracker_db_pub:new_user(Username, Password) of
      already_exists ->
         client:warning("email already exists");
      error ->
         client:warning("registration error");
      UserInfo ->
         user:save_into_session(UserInfo),
         wf:redirect("/")
   end.
