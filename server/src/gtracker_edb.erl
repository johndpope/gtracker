-module(gtracker_edb).

-behaviour(mds_gen_server).

-export([start/1, stop/0, on_start/1, on_stop/2, on_msg/3, on_amsg/2, on_info/2]).

-import(mds_utils, [get_param/2, get_param/3]).

-include("fields.hrl").

-define(MOD, {global, ?MODULE}).
-define(DEF_FAILOVER_PERIOD, 10).
-define(DEF_TABLE_DIR, ".").
-define(DEF_KEY_POS, 2).

-record(state, {}).

-record(device, {id, name, alias, online, timezone, registered, color, weight, pixmap, twitter_auth, triggers}).
-record(trigger, {enabled, name, type, use_email, email, use_phone, phone, use_twitter, text, config, schedule}).
-record(user, {id, name, password}).

%=======================================================================================================================
%  public exports
%=======================================================================================================================
% Opts = [Option]
% Option = {dump_timeout, Int()} | {auto_unload, Int()} | {failover_time, Int()}
%  dump_timeout   - see gtracker_table for details
%  auto_unload    - see gtracker_table for details
%  failover_time  - period of time in seconds after what track become closed, else this track will be reopened
start(Opts) ->
   mds_gen_server:start(?MOD, Opts).

stop() ->
   mds_gen_server:stop(?MOD).

%=======================================================================================================================
%  callbacks
%=======================================================================================================================
on_start(Opts) ->
%   crypto:start(),
%   ServerOpts = get_param(mds_server, Opts),
%   SelfOpts = get_param(self, Opts, []),
%   WorkingDir = filename:join(get_param(root_dir, ServerOpts), get_param(table_dir, SelfOpts, ?DEF_TABLE_DIR)),
%   mnesia:start(),
%   Res = (catch mnesia:table_info(device, all)),
%   case Res of
%      {'EXIT',{aborted,{no_exists,device,all}}} ->
%         create_schema();
%      _ -> ok
%   end,
%   log(info, "Mnesia started."),
   {ok, #state{}}.

on_stop(Reason, _State) ->
   crypto:stop(),
   mnesia:stop(),
   log(info, "Mnesia stopped."),
   log(info, "Stopped <~p>.", [Reason]),
   ok.

on_msg(stop, _From, State) ->
   {stop, normal, stopped, State};

on_msg(Msg, _From, State) ->
{reply, ok, State}.

on_amsg(Msg, State) ->
   log(error, "Unknown async message ~p.", [Msg]),
   {noreply, State}.

on_info(Msg, State) ->
   log(error, "Unknown info message ~p.", [Msg]),
   {noreply, State}.

%=======================================================================================================================
%  log helpers
%=======================================================================================================================
log(LogLevel, Format, Data) ->
   mds_gen_server:log(?MODULE, LogLevel, Format, Data).

log(LogLevel, Text) ->
   mds_gen_server:log(?MODULE, LogLevel, Text).

%=======================================================================================================================
%  tools
%=======================================================================================================================
create_schema() ->
   mnesia:stop(),
   mnesia:create_schema([]),
   mnesia:start(),
   mnesia:create_table(device, [{disc_copies, [node()]}, {type, ordered_set}, {attributes, record_info(fields, device)}]).
%   mnesia:create_table(track, [{disc_copies, [node()]}, {type, ordered_set}, {attributes, record_info(fields, track)}]),
%   mnesia:create_table(active_track, [{type, ordered_set}, {attributes, record_info(fields, active_track)}]).

%=======================================================================================================================
%  unit testing facilities
%=======================================================================================================================
%-ifdef(TEST).
%-include_lib("eunit/include/eunit.hrl").
%
%-endif.
