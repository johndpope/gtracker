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
   body(wf:session(admin)).

body(undefined) ->
   #template { file="./site/templates/logon_first.html" };

body(0) ->
   #template { file="./site/templates/denied.html" };

body(1) ->
   #panel { class=?MODULE, body=[
         #panel { class=view, body=select() },
         #panel { class=view, body=editor() }
      ]}.

select() ->
   [
      #h2 { text="News list:" },
      case q:exec(?SHOW_ALL_NEWS) of
         ?RESULT(News) ->
            display(News);
         _ ->
            "No news"
      end
   ].

display(News) ->
   Map = [
      timestamp@text,
      post@text,
      delete@postback
   ],
   #table { rows=[
         #tablerow { class=table_header, cells=
            [
               #tableheader { style="width: 15%;", text="Timestamp" },
               #tableheader { style="width: 80%;", text="Post" },
               #tableheader { style="width: 5%;" }
            ]
         },
         #bind { data=News, map=Map, transform=fun convert_row/2, body=#tablerow { class=record, cells=
               [
                  #tablecell { id=timestamp },
                  #tablecell { id=post },
                  #tablecell { body=#button { id=delete, text="Delete" } }
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
      #h2 { text="News list:" },
      #panel { body=[
            #span { text="Timestamp:" }, #br {},
            #textbox { id=timestamp, text=wf:f(?DATETIME_FORMAT, [Year, Month, Day, Hour, Minutes, Seconds]) }, #br {},
            #span { text="Post: " }, #br {},
            #textarea { id=post }, #p {},
            #button { text="Publicate", postback=publicate }
         ]}
   ].

event(publicate) ->
   Timestamp = wf:q(timestamp),
   Post = wf:q(post),
   q:exec(?CREATE_NEWS, [Timestamp, Post]),
   wf:redirect(wf:path_info());

event({delete, Id}) ->
   q:exec(?REMOVE_NEWS, [Id]),
   wf:redirect(wf:path_info()).
