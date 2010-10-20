-module(news_editor).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include("db.hrl").
-define(DATETIME_FORMAT, "~w-~w-~w ~w:~w:~w").

main() ->
   #template { file="./site/templates/index.html" }.

title() ->
   "GTracker - News Editor".

body() ->
   body(wf:user()).

body(undefined) ->
   "Authenticate first";

body(User) when User == "inndie@gmail.com" ->
   [
      #panel { class=edit, body=select() },
      #panel { class=edit, body=editor() }
   ];

body(_User) ->
   "News management disabled for you".

select() ->
   case q:exec(?SHOW_ALL_NEWS) of
      ?RESULT(News) ->
         display(News);
      _ ->
         "No news"
   end.

display(News) ->
   Map = [
      timestampLabel@text,
      postLabel@text,
      deleteButton@postback
   ],
   #table { rows=[
         #tablerow { cells=
            [
               #tableheader { style="width: 15%;", text="Timestamp" },
               #tableheader { style="width: 80%;", text="Post" },
               #tableheader { style="width: 5%;" }
            ]
         },
         #bind { data=News, map=Map, transform=fun convert_row/2, body=#tablerow { cells=
               [
                  #tablecell { id=timestampLabel },
                  #tablecell { id=postLabel },
                  #tablecell { body=#button { id=deleteButton, text="Delete" } }
               ]
            }
         }
      ]
   }.

convert_row(DataRow, Acc) ->
   [Id, {datetime,{{Year,Month,Day},{Hour,Minutes,Seconds}}}, Post] = DataRow,
   { [wf:f(?DATETIME_FORMAT, [Year, Month, Day, Hour, Minutes, Seconds]), Post, {delete, Id}], Acc, [] }.

editor() ->
   {{Year,Month,Day},{Hour,Minutes,Seconds}} = erlang:localtime(),
   [
      #label { text="Timestamp:" },
      #textbox { id=timestamp, style="width: 400px;", text=wf:f(?DATETIME_FORMAT, [Year, Month, Day, Hour, Minutes, Seconds]) }, #br {},
      #label { text="Post: " },
      #textarea { id=post, style="width: 400px; height: 300px;" }, #br {},
      #button { text="Publicate", postback=publicate }
   ].

event(publicate) ->
   Timestamp = wf:q(timestamp),
   Post = wf:q(post),
   q:exec(?CREATE_NEWS, [Timestamp, Post]),
   wf:redirect(wf:path_info());

event({delete, Id}) ->
   q:exec(?REMOVE_NEWS, [Id]),
   wf:redirect(wf:path_info()).
