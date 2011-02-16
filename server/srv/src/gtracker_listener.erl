-module(gtracker_listener).

-author(dmitryme).

-behaviour(mds_gen_server).

-include_lib("stdlib/include/qlc.hrl").
-include("common_defs.hrl").

-export([start/1, stop/1, on_start/1, on_stop/2, on_msg/3, on_amsg/2, on_info/2]).

-import(mds_utils, [get_param/2, get_param/3]).
-import(gtracker_common, [get_best_process/1, join_pg/2, leave_pg/2, send_metric/2]).


-define(TIMEOUT, 1).
-define(PORT, 7777).
-define(ADDRESS, "gtracker.ru").

-record(state, {
      name,                % registred instance name. E.g. {global, Name} see start/1
      group,               % process grup name
      timer_ref,           % reference of metric timer
      metric_send_period,  % period of sending metrics in ms
      lsocket,             % opened socker handler
      mt,                  % metadata globally registered name
      protocol,            % protocol implementation
      port,                % bind port
      host,                % running host name
      opts,                % all options
      active_clients = 0,  % number of active clients
      is_master = false    % true  - process can redirect to best node, false - passive
   }).

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
   MT = get_param(mt, SelfOpts),
   Proto = get_param(protocol, SelfOpts, gtracker_protocol),
   Group = get_param(group, SelfOpts, listener),
   MetricSendPeriod = get_param(metric_send_period, SelfOpts, ?def_metric_send_period),
   IsMaster = get_param(is_master, SelfOpts, false),
   join_pg(Group, self()),
   {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {packet, 1}, {reuseaddr, true}, {active, once}]),
   {ok, TimerRef} = timer:send_interval(MetricSendPeriod, self(), send_metric),
   process_flag(trap_exit, true),
   State = #state{
      name = ServName,
      group = Group,
      metric_send_period = MetricSendPeriod,
      timer_ref = TimerRef,
      lsocket = ListenSocket,
      mt = MT,
      protocol = Proto,
      port = Port,
      host = Host,
      is_master = IsMaster,
      opts = Opts},
   log(State, info, "Started ~p", [self()]),
   {ok, State, ?TIMEOUT}.

on_stop(Reason, State) ->
   leave_pg(State#state.group, self()),
   log(State, info, "Stopped <~p>.", [Reason]),
   ok.

on_msg(get_info, _From, State) ->
   {reply, [{host, State#state.host}, {port, State#state.port}], State, ?TIMEOUT};

on_msg(stop, _From, State) ->
   {stop, normal, stopped, State};

on_msg(_Msg, _Who, State) ->
   {norepy, State, ?TIMEOUT}.

on_amsg({log, LogLevel, Text}, State) ->
   log(State, LogLevel, Text),
   {noreply, State, ?TIMEOUT};

on_amsg({log, LogLevel, Format, Params}, State) ->
   log(State, LogLevel, Format, Params),
   {noreply, State, ?TIMEOUT};

on_amsg(_Msg, State) ->
   {norepy, State, ?TIMEOUT}.

on_info(send_metric, State = #state{name = Name, active_clients = AC}) ->
   [{message_queue_len, MQL}, {memory, M}] = process_info(self(), [message_queue_len, memory]),
   CpuUtil = cpu_sup:util(),
   Now = now(),
   send_metric(?metric_collector,
     [
        {Name, Now, ?message_queue_len, MQL},
        {Name, Now, ?memory, M},
        {net_adm:localhost(), Now, ?cpu, CpuUtil},
        {Name, Now, ?active_clients, AC}
     ]),
   {noreply, State, ?TIMEOUT};

on_info({'EXIT', Pid, Reason}, State = #state{mt = Mt, active_clients = AC}) ->
   log(State, info, "Process ~p exited with status ~p. Metadata will be notified.", [Pid, Reason]),
   gen_server:cast(Mt, {exited, Pid}),
   {noreply, State#state{active_clients = AC - 1}, ?TIMEOUT};

on_info(timeout, State = #state{name = Name, group = Group, is_master = IsMaster, active_clients = AC}) ->
   ListenSocket = State#state.lsocket,
   case gen_tcp:accept(ListenSocket, 0) of
      {ok, PeerSocket} ->
         {ok, Addr} = inet:peername(PeerSocket),
         log(State, info, "Device connected from ~p.", [Addr]),
         {ok, BestProcess} = if (IsMaster == true) -> gtracker_common:get_best_process(Group); true -> {ok, self()} end,
         if (BestProcess ==  self()) ->
            apply(State#state.protocol, start_link, [PeerSocket, [{listener, Name}, {opts, State#state.opts}]]),
            {noreply, State#state{active_clients = AC + 1}, ?TIMEOUT};
         true ->
            ConnectionInfo = gen_server:call(BestProcess, get_info),
            log(State, info, "Best listener ~p with pid ~p found. The client will  be redirected.", [ConnectionInfo,
                  BestProcess]),
            apply(State#state.protocol, reconnect_to, [PeerSocket, ConnectionInfo]),
            {noreply, State, ?TIMEOUT}
         end;
      {error, timeout} ->
         {noreply, State, ?TIMEOUT}
   end;

on_info(Msg, State) ->
   log(State, error, "Unknown message ~p received", [Msg]),
   {noreply, State, ?TIMEOUT}.

log(#state{name = N}, LogLevel, Format, Data) ->
   mds_gen_server:log(N, LogLevel, Format, Data).

log(#state{name = N}, LogLevel, Text) ->
   mds_gen_server:log(N, LogLevel, Text).
