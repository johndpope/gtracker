-module(news_editor).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include_lib("gtracker_db_pub/include/common_recs.hrl").
-include("defines.hrl").

main() ->
   #template { file="./site/templates/index.html" }.

title() ->
   "News Editor".

render() ->
   render(user:admin()).

render(undefined) ->
   #template { file="./site/templates/messages/logon_first" };

render(false) ->
   #template { file="./site/templates/messages/denied" };

render(true) ->
   #panel { class=?MODULE, body=[
         #panel { class=view, body=select() },
         #panel { class=view, body=editor() }
      ]}.

select() ->
   [
      #h2 { text="News list:" },
      case gtracker_db_pub:get_news() of
         invalid_date ->
            "Internal error: invalid date";
         [] ->
            "No news";
         News ->
            show(News)
      end
   ].

show(News) ->
   Map = [
      date@text,
      post@text,
      delete@postback
   ],
   #table { rows=[
         #tablerow { class=table_header, cells=
            [
               #tableheader { style="width: 15%;", text="Date" },
               #tableheader { style="width: 80%;", text="Post" },
               #tableheader { style="width: 5%;" }
            ]
         },
         #bind { data=News, map=Map, transform=fun convert_row/2, body=#tablerow { class=record, cells=
               [
                  #tablecell { id=date },
                  #tablecell { id=post },
                  #tablecell { body=#button { id=delete, text="Delete" } }
               ]
            }
         }
      ]
   }.

convert_row(_DataRow = #news { id=NewsID, date={Year, Month, Day}, text=Post }, Acc) ->
   { [wf:f("~2.10.0B/~2.10.0B/~w", [Day, Month, Year]), Post, {delete_news, NewsID}], Acc, [] }.

editor() ->
   {{Year,Month,Day},{_Hour,_Minutes,_Seconds}} = erlang:localtime(),
   [
      #h2 { text="News list:" },
      #panel { body=[
            #span { text="Date:" }, #br {},
            #textbox { id=date, text=wf:f(?DATE_FMT, [Day, Month, Year]) }, #br {},
            #span { text="Post: " }, #br {},
            #textarea { id=post }, #p {},
            #button { text="Publicate", postback=insert_news }
         ]}
   ].

event(insert_news) ->
   F = fun() ->
         {ok, [Day, Month, Year], []} = io_lib:fread("~2d/~2d/~4d", wf:q(date)),
         {Year, Month, Day}
   end,
   try F() of
      Date ->
         Post = wf:q(post),
         case gtracker_db_pub:insert_news(Date, Post) of
            error ->
               client:error("Failed to insert news");
            RefID ->
               wf:info("News inserted = [~p]", [RefID]),
               wf:redirect(wf:path_info())
         end
   catch
      _:_Err ->
         client:error("Wrong date")
   end;

event({delete_news, RefID}) ->
   case gtracker_db_pub:delete_news(RefID) of
      error ->
         client:error("Faield to remove news");
      ok ->
         wf:redirect(wf:path_info())
   end.
