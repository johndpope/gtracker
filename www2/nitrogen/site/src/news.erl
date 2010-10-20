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
      timestampLabel@text,
      postLabel@text
   ],

   #bind { data=News, map=Map, transform=fun convert_row/2, body=
      [
         "<div class=\"newsWrapper\">",
         #panel { class=news, body=[
               #label { class=timestamp, id=timestampLabel },
               #label { class=post, id=postLabel }
            ]},
         "</div>"
      ]
   }.

convert_row(DataRow, Acc) ->
   [{datetime,{{Year,Month,Day},{_Hour,_Minutes,_Seconds}}}, Post] = DataRow,
   { [wf:f("~w/~w/~w", [Day, Month, Year]), Post], Acc, [] }.
