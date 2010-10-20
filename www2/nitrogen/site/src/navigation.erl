-module(navigation).
-include_lib("nitrogen/include/wf.hrl").

-export([display/0]).

display() ->
   [
      #list { class=navigation, body=[
            #listitem { body=#link { text="Map", url="/map" }},
            #listitem { body=#link { text="Download", url="/download" }},
            #listitem { body=#link { text="Contacts", url="/contacts" }}
         ]}
   ].
