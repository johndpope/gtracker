-module(gtracker_listener).

-author(dmitryme).

-behaviour(mds_gen_server).

-include_lib("stdlib/include/qlc.hrl").
-include("common_defs.hrl").

-export([start/1, stop/1, on_start/1, on_stop/2, on_msg/3, on_amsg/2, on_info/2]).

-import(mds_utils, [get_param/2, get_param/3]).
-import(gtracker_common, [get_best_process/1, join_pg/2, leave_pg/2]).


-define(TIMEOUT, 100).
-define(PORT, 7777).
-define(ADDRESS, "gtracker.ru").

-record(state, {name, group, lsocket, db, protocol, port, host, opts}).

start(Opts) ->
   SelfOpts = get_param(self, Opts),
   ServName = {global, _} = get_param(name, SelfOpts), % should have {global, Atom()} format
   mds_gen_server:start(ServName, ?MODULE, Opts).

stop(State) ->
   mds_gen_server:stop(State#state.name).

on_start(Opts) ->
   SelfOpts = get_param('self', Opts),
   Port = get_param(port, SelfOpts, ?PORT),
   Host = get_param(host, SelfOpts, ?ADDRESS),
   ServName = get_param(name, SelfOpts),
   Db = get_param(db, SelfOpts),
   Proto = get_param(protocol, SelfOpts, gtracker_protocol),
   Group = get_param(group, SelfOpts, listener),
   join_pg(Group, self()),
   {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {packet, 1}, {reuseaddr, true}, {active, once}]),
   {ok, #state{name = ServName, group = Group, lsocket = ListenSocket, db = Db, protocol = Proto, port = Port, host = Host, opts = Opts}, ?TIMEOUT}.

on_stop(Reason, State) ->
   leave_pg(State#state.group, self()),
   log(info, "Stopped <~p>.", [Reason]),
   ok.

on_msg(get_info, _From, State) ->
   {reply, [{host, State#state.host}, {port, State#state.port}], State};

on_msg(stop, _From, State) ->
   {stop, normal, stopped, State};

on_msg(_Msg, _Who, State) ->
   {norepy, State, 0}.

on_amsg({log, LogLevel, Text}, State) ->
   log(LogLevel, Text),
   {noreply, State, 0};

on_amsg({log, LogLevel, Format, Params}, State) ->
   log(LogLevel, Format, Params),
   {noreply, State, 0};

on_amsg(_Msg, State) ->
   {norepy, State, 0}.

on_info(_Msg, State = #state{name = Name, group = Group}) ->
   ListenSocket = State#state.lsocket,
   case gen_tcp:accept(ListenSocket, 0) of
      {ok, PeerSocket} ->
         {ok, Addr} = inet:peername(PeerSocket),
         log(info, "Device connected from ~p.", [Addr]),
         {ok, BestProcess} = gtracker_common:get_best_process(Group),
         if (BestProcess ==  self()) ->
            apply(State#state.protocol, start_link, [PeerSocket, [{listener, Name}, {opts, State#state.opts}]]),
            {noreply, State, ?TIMEOUT};
         true ->
            ConnectionInfo = gen_server:call(BestProcess, get_info),
            apply(State#state.protocol, reconnect_to, [PeerSocket, ConnectionInfo]),
            {noreply, State, ?TIMEOUT}
         end;
      {error, timeout} ->
         {noreply, State, ?TIMEOUT}
   end;

on_info({'EXIT', Pid, Reason}, State = #state{db = Db}) ->
   log(info, "Process ~p exited with status ~p. Db will be notified.", [Pid, Reason]),
   gen_server:cast(Db, {exited, Pid}),
   {noreply, State}.

log(LogLevel, Format, Data) ->
   mds_gen_server:log(?MODULE, LogLevel, Format, Data).

log(LogLevel, Text) ->
   mds_gen_server:log(?MODULE, LogLevel, Text).
