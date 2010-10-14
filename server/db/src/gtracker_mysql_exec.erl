-module(gtracker_mysql_exec).

-export([
         start/6
         ,stop/0
         ,start_tran/0
         ,commit_tran/0
         ,rollback_tran/0
         ,insert_coord/3
         ,select_last_track/1
         ,stop_track/1
         ,reopen_track/1
         ,new_track/1
         ,new_track/2
         ,rename_track/2
         ,update_track/2
         ,add_reference/3
         ,select_triggers/1
         ,select_device/1
         ,insert_device/1
         ,set_online/1
         ,set_offline/1
      ]).

-import(gtracker_common, [datetime_to_unix_seconds/1, binary_to_hex/1]).

-include("common_defs.hrl").

-define(MAX_FETCH_TIMEOUT, 60000).

-define(DBPOOL,       mysqldb                                     ).
-define(COUNT(A),     {data, {mysql_result, _, [[A]], _, _}}      ).
-define(RESULT(Var),  {data, {mysql_result, _, Var, _, _}}        ).
-define(UPDATED(Var), {updated, {mysql_result, _, _, Var, _}}     ).
-define(ERROR(Var),   {error, {mysql_result, _, _, _, Var}}       ).

-define(START_TRAN, execute("start transaction;", [])).
-define(COMMIT_TRAN, execute("commit;", [])).
-define(ROLLBACK_TRAN, execute("rollback;", [])).

-define(SELECT_DEVICE, "select d.id, d.name, d.alias, r.value as ref, d.online, d.timezone, d.twitter_auth "
                        "from device as d "
                        "inner join reference as r on (d.id = r.device_id) "
                        "where r.track_id is NULL and d.name='~s';").

-define(INSERT_DEVICE, "insert into device(name) values('~s');").

-define(INSERT_COORD,  "insert into coordinate(track_id, device_id, latitude, longitude, speed, time) "
                       "values(~p, ~p, ~p, ~p, ~p, FROM_UNIXTIME(~p)); ").

-define(DEVICE_ONLINE, "update device set online = ~p where device.name='~s';").

-define(SELECT_LAST_TRACK,  "select t.id as tid, max(c.time) as time "
                            "from track as t "
                            "left outer join coordinate as c on t.id = c.track_id "
                            "where t.device_id = ~p "
                            "group by tid order by tid desc limit 1").

-define(INSERT_NEW_TRACK, "insert into track(device_id) "
                          "values(~p);").

-define(INSERT_NEW_TRACK2, "insert into track(device_id, name) "
                           "values(~p, ~p);").

-define(UPDATE_TRACK,   "update track "
                        "set length = length + ~p, "
                        "avg_speed = (select avg(speed) from coordinate where track_id = ~p) "
                        "where id = ~p;").

-define(CLOSE_TRACK, "update track set status = 'closed' where id = ~p").
-define(OPEN_TRACK, "update track set status = 'opened' where id = ~p").

-define(INSERT_REFERENCE, "insert into reference(device_id, value) values(~p,~p);").
-define(INSERT_REFERENCE2, "insert into reference(device_id, track_id, value) values(~p,~p,~p);").

-define(SELECT_TRIGGERS,   "select t.id, t.name, type, t.use_email, t.email, t.use_phone, t.phone, t.use_twitter, t.text, t.config, t.schedule "
                           "from `trigger` as t "
                           "inner join device on t.device_id = device.id "
                           "where device.name = ~p and t.enabled=TRUE;").

-define(RENAME_TRACK, "update track set name = ~p where id = ~p;").


% start(Host, Post, User, Password, DbName, LogCallback) -> ok
%    Host = String()
%    Port = Int()
%    User = String()
%    Password = String()
%    DbName = String()
%    LogCallback = fun/4
start(Host, Port, User, Password, DbName, LogCallback) ->
   mysql:start_link(?DBPOOL, Host, Port, User, Password, DbName, LogCallback),
   ok.

stop() ->
   ok.

% start_tran() -> ok
%    throws Error()
start_tran() ->
   execute("start transaction;", []).

% commit_tran() -> ok
%    throws Error()
commit_tran() ->
   execute("commit;", []).

% rollback_tran() -> ok
%    throws Error()
rollback_tran() ->
   execute("rollback;", []).

% insert_coord(TrackId, DevId, Coord) -> true | false | Error()
%     TrackId = Int()
%     DevId = Int()
%     Coord = tuple
%    throws Error()
insert_coord(TrackId, DevId, {Lat, Lon, Speed, _Distance, Timestamp}) ->
   execute(?INSERT_COORD, [TrackId, DevId, Lat, Lon, Speed, datetime_to_unix_seconds(Timestamp)]).

% select_last_track(DevId) -> no_track | {TrackId, undef} | {Track, LastCoordTm}
%    DevId = Int()
%    TrackId = Int()
%    LastCoordTm = Int(), seconds since Epoc
%    throws Error()
select_last_track(DevId) ->
   case execute(?SELECT_LAST_TRACK, [DevId]) of
      ?RESULT([]) ->
         no_track;
      ?RESULT([[undefined, undefined]]) ->
         no_track;
      ?RESULT([[TrackId, undefined]]) -> % empty track
         {TrackId, undef};
      ?RESULT([[TrackId, {datetime, LastCoordTm}]]) ->
         {TrackId, LastCoordTm}
   end.

% stop_track(TrackId) -> true | false
%    TrackId = Int()
%    throws Error()
stop_track(TrackId) ->
   case execute(?CLOSE_TRACK, [TrackId]) of
      ?UPDATED(1) ->
         true;
      _ ->
         false
   end.

% reopen_track(TrackId) -> true | false
%    TrackId = Int()
%    throws Error()
reopen_track(TrackId) ->
   case execute(?OPEN_TRACK, [TrackId]) of
      ?UPDATED(1) ->
         true;
      _ ->
         false
   end.

% new_track(DevId) -> Int()
%    DevId = Int()
%    throws Error()
new_track(DevId) ->
   execute(?INSERT_NEW_TRACK, [DevId]),
   get_last_inserted_id().

% new_track(DevId, TrackName) -> Int()
%    DevId = Int()
%    TrackName = String()
%    throws Error()
new_track(DevId, TrackName) ->
   execute(?INSERT_NEW_TRACK2, [DevId, TrackName]),
   get_last_inserted_id().

% rename_track(TrackId, TrackName) -> true | false
%    TrackId = Int()
%    TrackName = String()
%    throws Error()
rename_track(TrackId, TrackName) ->
   case execute(?RENAME_TRACK, [TrackName, TrackId]) of
      ?UPDATED(1) ->
         true;
      _ ->
         false
   end.

% update_track(TrackId, Distance) -> true | false
%    TrackId = Int()
%    Distance = Int(), in meters
%    throws Error()
update_track(TrackId, Distance) ->
   case execute(?UPDATE_TRACK, [Distance, TrackId, TrackId]) of
      ?UPDATED(1) ->
         true;
      _ ->
         false
   end.

% add_reference(DevId, TrackId, Reference) -> Int()
%    DevId = Int()
%    TrackId = Int()
%    Reference = String
%    throws Error()
add_reference(DevId, TrackId, Reference) ->
   execute(?INSERT_REFERENCE2, [DevId, TrackId, Reference]),
   get_last_inserted_id().

% select_triggers(DevName) -> [trigger]
%    DevName = String()
%    throws Error()
select_triggers(DevName) ->
   case execute(?SELECT_TRIGGERS, [DevName]) of
      ?RESULT([]) ->
         no_triggers;
      ?RESULT(Res) ->
         btrigger_to_term(Res) % convert triggers
   end.

% select_device(DevName) -> no_device | {DevId, Alias, Ref, Online}
%    DevName = String()
%    DevId = Int()
%    Alias = undef | String()
%    Online = online | offline
%    Ref = String()
%    throws Error()
select_device(DevName) ->
   State_to_atom = fun(0) -> offline;
                   (1) -> online
                end,
   case execute(?SELECT_DEVICE, [DevName]) of
      ?RESULT([]) ->
         no_device;
      ?RESULT([[DevId, Name, Alias, Ref, Online, Timezone, TwitterAuth]]) ->
         #device{
            id = DevId,
            name = bin_to_list(Name),
            alias = bin_to_list(Alias),
            reference = Ref,
            online = State_to_atom(Online),
            timezone = bin_to_list(Timezone),
            twitter_auth = bin_to_term(TwitterAuth)}
   end.

% insert_device(DevName) -> Int()
%    DevName = String()
%    throws Error()
insert_device(DevName) ->
   execute(?INSERT_DEVICE, [DevName]),
   DevId = get_last_inserted_id(),
   Ref = binary_to_hex(erlang:md5(<<DevId>>)),
   execute(?INSERT_REFERENCE, [DevId, Ref]),
   DevId.

% set_online(DevName) -> true | false
%     DevName = String(), device name
%    throws Error()
set_online(DevName) ->
   case execute(?DEVICE_ONLINE, [1, DevName]) of
      ?UPDATED(1) ->
         true;
      _ ->
         false
   end.

% set_offline(DevName) -> true | false
%     DevName = String(), device name
%    throws Error()
set_offline(DevName) ->
   case execute(?DEVICE_ONLINE, [0, DevName]) of
      ?UPDATED(1) ->
         true;
      _ ->
         false
   end.

%=======================================================================================================================
%  internals
%=======================================================================================================================
execute(QueryText, Params) ->
   Query = io_lib:format(QueryText, Params),
   execute(Query).

execute(Query) ->
   Res = mysql:fetch(?DBPOOL, Query, ?MAX_FETCH_TIMEOUT),
   case Res of
      ?ERROR(ErrTxt) ->
         throw({error, Query, erlang:bitstring_to_list(ErrTxt)});
      Result ->
         Result
   end.

get_last_inserted_id() ->
   ?RESULT([[Id]]) = execute("select last_insert_id();"),
   Id.

% Binary string to Erlang list
% bin_to_list(Binary()) -> undef | String()
bin_to_list(undefined) ->
   undef;
bin_to_list(BinStr) ->
   erlang:binary_to_list(BinStr).
% Binary string to atom
% bstr_to_atom(Binary()) -> undef | atom()
bstr_to_atom(undefined) ->
   undef;
bstr_to_atom(BinStr) ->
   mds_utils:list_to_atom(erlang:binary_to_list(BinStr)).

bin_to_bin(undefined) ->
   undef;
bin_to_bin(Binary) ->
   Binary.

bin_to_term(undefined) ->
   undef;
bin_to_term(Binary) ->
   F = fun() ->
         {ok, Tokens, _} = erl_scan:string(erlang:binary_to_list(Binary)),
         {ok, Term} = erl_parse:parse_term(Tokens),
         Term
      end,
   try F() of
      Term ->
         Term
   catch
      _:_Err ->
         throw({error, "Unable to parse " ++ erlang:binary_to_list(Binary)})
   end.

str_time_to_int([H1, H2, $:, M1, M2]) ->
   IntHH = erlang:list_to_integer([H1, H2]),
   IntMM = erlang:list_to_integer([M1, M2]),
   3600 * IntHH + 60 * IntMM.

bin_to_schedule(undefined) ->
   always;
bin_to_schedule(BinSchedule) when is_binary(BinSchedule) ->
   bin_to_schedule(bin_to_term(BinSchedule));
bin_to_schedule([]) ->
   [];
bin_to_schedule([{Day, Periods} | Rest]) ->
   IntPeriods = str_to_int_periods(Periods),
   [{day_of_week(Day), IntPeriods} | bin_to_schedule(Rest)].

day_of_week(mon) -> 1;
day_of_week(tue) -> 2;
day_of_week(wed) -> 3;
day_of_week(thu) -> 4;
day_of_week(fri) -> 5;
day_of_week(sat) -> 6;
day_of_week(sun) -> 7;
day_of_week(_) -> throw(unknown_day_of_week).

type_to_ready(leave) -> false;
type_to_ready(enter) -> false;
type_to_ready(_) -> true.

str_to_int_periods([]) ->
   [];
str_to_int_periods([{From, Till} | Rest]) ->
   [{str_time_to_int(From), str_time_to_int(Till)} | str_to_int_periods(Rest)].

bin_to_bool(0) -> false;
bin_to_bool(1) -> true.

btrigger_to_term([]) ->
   [];
btrigger_to_term([[Id, BinName, BinType, UseEmail, BinEmail, UsePhone, BinPhone, UseTwitter, BinText, BinConfig, BinSchedule]|Res]) ->
   Type = bstr_to_atom(BinType),
   [#trigger{id = Id,
             enabled = true,
             name = bin_to_list(BinName),
             type = Type,
             ready = type_to_ready(Type),
             email = #email{enabled = bin_to_bool(UseEmail), value = bin_to_list(BinEmail)},
             sms   = #sms{enabled = bin_to_bool(UsePhone),  value = bin_to_list(BinPhone)},
             twitter =  bin_to_bool(UseTwitter),
             text = bin_to_bin(BinText),
             config = bin_to_term(BinConfig),
             schedule = bin_to_schedule(BinSchedule)} | btrigger_to_term(Res)].

%=======================================================================================================================
%  unit testing facilities
%=======================================================================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

str_time_to_int_test() ->
   ?assertEqual(43800, str_time_to_int("12:10")),
   ?assertEqual(600, str_time_to_int("00:10")).

bin_to_schedule_test() ->
   BinSched = <<"[{sun, [{\"00:10\", \"12:00\"}, {\"15:00\", \"16:00\"}]}, {fri, [{\"08:15\", \"09:30\"}, {\"10:30\",
   \"11:30\"}, {\"14:15\", \"16:45\"}]}].">>,
   ?assertEqual([{7,[{600,43200},{54000,57600}]}, {5,[{29700,34200},{37800,41400},{51300,60300}]}], bin_to_schedule(BinSched)).

bin_to_term_test() ->
   Binary = <<"[1,2,3,4]">>,
   Res = (catch bin_to_term(Binary)),
   ?assertEqual({error, "Unable to parse [1,2,3,4]"}, Res),
   ?assertEqual([{sun, [{"13:00", "15:00"}]}], bin_to_term(<<"[{sun, [{\"13:00\", \"15:00\"}]}].">>)).
-endif.
