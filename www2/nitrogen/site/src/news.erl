-module(news).

-include_lib("nitrogen/include/wf.hrl").
-include("db.hrl").

-export([display/0]).

display() ->
   case q:exec(?SHOW_NEWS) of
      ?RESULT(News) ->
         display(News);
      _ ->
         "No news"
   end.

display(News) ->
   Map = [
      timestamp@text,
      post@body
   ],

   #bind { data=News, map=Map, transform=fun convert_row/2, body=
      [
         "<div class=\"news_wrapper\">",
         #panel { class=news, body=[
               #h3 { id=timestamp },
               #p { id=post }
            ]},
         "</div>"
      ]
   }.

convert_row(DataRow, Acc) ->
   [{datetime,{{Year,Month,Day},{_Hour,_Minutes,_Seconds}}}, Post] = DataRow,
   { [wf:f("~2.10.0B/~2.10.0B/~w", [Day, Month, Year]), Post], Acc, [] }.
