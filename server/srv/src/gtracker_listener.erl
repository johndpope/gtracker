-module(gtracker_listener).

-author(dmitryme).

-behaviour(mds_gen_server).

-include_lib("stdlib/include/qlc.hrl").
-include("common_defs.hrl").

-export([start/1, stop/0, on_start/1, on_stop/2, on_msg/3, on_amsg/2, on_info/2]).

-export([start_in_shell/0]).

-import(mds_utils, [get_param/2, get_param/3]).


-define(TIMEOUT, 100).
-define(PORT, 7777).
-define(ADDRESS, "gtracker.ru").
-define(MOD, {global, ?MODULE}).

-record(state, {lsocket, db, protocol, port, host, opts}).

start_in_shell() ->
   start([{root_dir, "/tmp/gtracker"}, {db, nodb}, {log_level, debug}]).

start(Opts) ->
   mds_gen_server:start(?MOD, Opts).

stop() ->
   mds_gen_server:stop(?MOD).

on_start(Opts) ->
   SelfOpts = get_param('self', Opts),
   Port = get_param(port, SelfOpts, ?PORT),
   Host = get_param(host, SelfOpts, ?ADDRESS),
   Db = get_param(db, SelfOpts),
   Proto = get_param(protocol, SelfOpts, gtracker_protocol),
   {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {packet, 1}, {reuseaddr, true}, {active, once}]),
   log(info, "Started"),
   {ok, #state{lsocket = ListenSocket, db = Db, protocol = Proto, port = Port, host = Host, opts = Opts}, ?TIMEOUT}.

on_stop(Reason, _State) ->
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

on_info(_Msg, State) ->
   ListenSocket = State#state.lsocket,
   case gen_tcp:accept(ListenSocket, 0) of
      {ok, PeerSocket} ->
         {ok, Addr} = inet:peername(PeerSocket),
         log(info, "Device connected from ~p.", [Addr]),
         Node = gtracker_common:get_best_node(true, []),
         if (node() == Node) ->
            apply(State#state.protocol, start, [PeerSocket, [{listener, ?MOD}, {opts, State#state.opts}]]),
            {noreply, State, ?TIMEOUT};
         true ->
            NodeInfo = gen_server:call(Node, gtracker_listener, [get_info]),
            apply(State#state.protocol, reconnect_to, [PeerSocket, NodeInfo]),
            {noreply, State, ?TIMEOUT}
         end;
      {error, timeout} ->
         {noreply, State, ?TIMEOUT}
   end.

log(LogLevel, Format, Data) ->
   mds_gen_server:log(?MODULE, LogLevel, Format, Data).

log(LogLevel, Text) ->
   mds_gen_server:log(?MODULE, LogLevel, Text).
