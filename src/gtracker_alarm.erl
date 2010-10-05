-module(gtracker_alarm).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-export([test_error_logger_add/0]).

-import(mds_utils, [get_param/2, get_param/3]).

-record(state, {to, subject, logger}).

test_error_logger_add() ->
   error_logger:add_report_handler(
      gtracker_alarm,
      [{mds_server, []}, {mds_logger, [{log_level, debug}, {enable_alarm, false}]}, {self, [{to, "dmitryme@gmail.com"}, {subject,
                  "gTracker "}]}]).

init(Opts) ->
   LoggerOpts = dict:from_list(get_param(mds_logger, Opts)),
   ServerOpts = get_param(mds_server, Opts),
   ModLoggerOpts = dict:store(enable_alarm, false, LoggerOpts),
   {ok, Pid} = mds_logger:start(alarm, dict:to_list(ModLoggerOpts) ++ ServerOpts),
   SelfOpts = get_param(self, Opts),
   To = get_param(to, SelfOpts),
   Subject = get_param(subject, SelfOpts),
   {ok, #state{to = To, subject = Subject, logger = Pid}}.

handle_event({error, _, {_, Text, []}}, State = #state{to = To, subject = Subject, logger = L}) ->
  FWhen = lists:flatten(getTime()),
  send_email(To, Subject ++ "ERROR", FWhen, "<none>", Text, L),
  {ok, State};
handle_event({error, _, {_, Format, Data}}, State = #state{to = To, subject = Subject, logger = L}) ->
  FText = lists:flatten(io_lib:format(Format, Data)),
  FWhen = lists:flatten(getTime()),
  send_email(To, Subject ++ "ERROR", FWhen, "<none>", FText, L),
  {ok, State};
handle_event({_, _, {_, alarm, {Level, When, From, Text}}}, State = #state{to = To, subject = Subject, logger = L}) ->
   FText = lists:flatten(Text),
   FFrom = lists:flatten(From),
   FWhen = lists:flatten(When),
   send_email(To, Subject ++ level2txt(Level), FWhen, FFrom, FText, L),
   {ok, State};
handle_event({warning, _, {_, Text, []}}, State = #state{to = To, subject = Subject, logger = L}) ->
  FWhen = lists:flatten(getTime()),
  send_email(To, Subject ++ "WARNING", FWhen, "<none>", Text, L),
  {ok, State};
handle_event({warning, _, {_, Format, Data}}, State = #state{to = To, subject = Subject, logger = L}) ->
  FText = lists:flatten(io_lib:format(Format, Data)),
  FWhen = lists:flatten(getTime()),
  send_email(To, Subject ++ "WARNING", FWhen, "<none>", FText, L),
  {ok, State};
handle_event(E, State = #state{logger = L}) ->
   mds_logger:log(L, info, E),
   {ok, State}.

handle_call(_R, State) ->
   {ok, ok, State}.

handle_info(_I, State) ->
   {ok, State}.

terminate(_A, _State) ->
   stop.

code_change(_, _, _) ->
   {ok, undef}.

send_email(To, Subject, When, From, Text, Logger) ->
   Body = "From: " ++ From ++ "\nWhen: " ++ When ++ "\n" ++ Text,
   case mds_utils:send_email(To, Subject, erlang:list_to_binary(Body)) of
      ok ->
         mds_logger:log(Logger, debug, "eMail was sent: To: ~p, Subject: '~p', Text: ~p", [To, Subject, Text]);
      {error, Reason} ->
         mds_logger:log(Logger, error, "Unable to send eMail: ~p", [Reason])
   end.

getTime() ->
   Now = {_, _, MicroSec} = now(),
   {{_,_,_},{HH,MM,SS}} = calendar:now_to_local_time(Now),
   io_lib:format("~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B", [HH, MM, SS, MicroSec div 1000]).

level2txt(error) ->
   "ERROR";
level2txt(warning) ->
   "WARNING";
level2txt(Rest) ->
   erlang:atom_to_list(Rest).
