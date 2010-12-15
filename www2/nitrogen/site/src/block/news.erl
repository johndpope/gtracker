-module(news).
-include_lib("nitrogen/include/wf.hrl").
-include_lib("gtracker_db_pub/include/common_recs.hrl").
-include("defines.hrl").
-export([render/0]).

render() ->
   case gtracker_db_pub:get_news() of
      invalid_date ->
         "Internal error: invalid date";
      [] ->
         "No news";
      News ->
         render(News)
   end.

render(News) ->
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

convert_row(_DataRow = #news { date={Year, Month, Day}, text=Post }, Acc) ->
   { [wf:f(?DATE_FMT, [Day, Month, Year]), Post], Acc, [] }.
