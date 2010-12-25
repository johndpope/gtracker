-module(gtracker_protocol).

-import(mds_utils, [get_param/2, get_param/3]).

-include("error_codes.hrl").
-include("fields.hrl").
-include("msgs.hrl").
-include("common_defs.hrl").
-include("common_recs.hrl").

-export([start/2, stop/1]).

-export([parsePacket/2, processMsg/3]).

-import(gtracker_common, [unix_seconds_to_datetime/1, ints_to_float/2, send_pg/2, fill_binary/3]).

-record(state, {dev = undef,
                track = undef,
                socket,         % device socket
                listener,       % registered name of gtracker_listener process
                db,             % registered name for database process
                logger = undef, % logger of this process
                ecnt = 0,       % count of errors
                ccnt = 0,       % number of received coordinated per session
                last_coord = undef, % last received coordinate
                calc_speed = false,
                ref_prefix,
                logger_opts     % options
             }).

-define(MAX_ERROR_COUNT, 10).
-define(RECEIVE_TIMEOUT,  60000).
-define(DEF_REF_PREFIX, "http://www.gtracker.ru/view/").

-compile({no_auto_import, [unregister/1]}).
%=======================================================================================================================
% gtracker_protocol exports
%=======================================================================================================================
start(Socket, Opts) ->
   Listener = get_param(listener, Opts),
   AllOpts = get_param(opts, Opts),
   ListenerOpts = get_param(self, AllOpts),
   CalcSpeed = get_param(calc_speed, ListenerOpts),
   Db = get_param(db, ListenerOpts),
   RefPrefix = get_param(ref_prefix, ListenerOpts, ?DEF_REF_PREFIX),
   % build logger options
   LoggerOpts = dict:from_list(get_param(mds_logger, AllOpts)),
   {ok, RootDir} = dict:find(root_dir, LoggerOpts),
   RootOpts = dict:store(root_dir, filename:join(RootDir, "devices"), LoggerOpts),
   Pid = spawn_link(
      fun() ->
            process_flag(trap_exit, true),
            loop(#state{
                  socket = Socket,
                  db = Db,
                  listener = Listener,
                  calc_speed = CalcSpeed,
                  ref_prefix = RefPrefix,
                  logger_opts = RootOpts})
      end),
   gen_tcp:controlling_process(Socket, Pid).

stop(Pid) ->
   Pid ! stop.

%=======================================================================================================================
% gtracker_protocol main loop
%=======================================================================================================================
loop(State = #state{dev = Device, track = Track, socket = Socket}) ->
   receive
      {tcp, Socket, Data} ->
         {ok, PeerName} = inet:peername(Socket),
         log(State, debug, "Packet ~p received from ~p.", [Data, PeerName]),
         {Repl, NewState} = parsePacket(Data, State),
         reply(State, Repl, Socket),
         inet:setopts(Socket, [{active, once}]),
         loop(NewState);
      {error, closed} ->
         log(State, error, "Device ~p with ID = ~p was closed.", [self(), Device#device.name]);
      stop ->
         log(State, info, "Device was forced to stop."),
         unregister(State),
         gen_tcp:close(Socket);
      {tcp_closed, _Socket} ->
         unregister(State),
         log(State, info, "Device ~p with ID = ~p was closed by peer.", [self(), Device#device.name]);
      {'EXIT', Pid, _} when State#state.track#track.pid =:= Pid ->
         log(State, info, "Track ~p has been crashed.", [Track#track.id]),
         loop(State#state{track = undef});
      {'EXIT', _, _} ->
         unregister(State),
         gen_tcp:close(State#state.socket),
         log(State, info, "Device was forced to stop.");
      {updated, D = #device{name = DevName}} when DevName =/= Device#device.name ->
         log(State, error, "updated(~p) received from alien device.", [D]),
         loop(State);
      {updated, NewDevice} when is_record(NewDevice, device) ->
         gtracker_track_pub:set_subscribers(Track, NewDevice#device.subs),
         loop(State#state{dev = NewDevice});
      Msg ->
         log(State, error, "Unknown message '~p' received.", [Msg])
   after ?RECEIVE_TIMEOUT ->
         log(State, error, "No data received in ~p interval.", [?RECEIVE_TIMEOUT]),
         self() ! stop,
         loop(State)
   end.

%=======================================================================================================================
% logger for listener
%=======================================================================================================================
log2listener(Listener, LogLevel, Text) ->
   mds_gen_server:cast(Listener, {log, LogLevel, Text}).

log2listener(Listener, LogLevel, Format, Params) ->
   mds_gen_server:cast(Listener, {log, LogLevel, Format, Params}).

%=======================================================================================================================
% create new logger for device
%=======================================================================================================================
create_logger(DevName, State = #state{logger = undef, logger_opts = LOpts}) ->
   Name = mds_utils:list_to_atom(DevName),
   process_flag(trap_exit, true),
   ClntLoggerOpts = dict:to_list(dict:store(working_dir, DevName, LOpts)),
   {ok, Pid} = mds_logger:start_link(Name, ClntLoggerOpts),
   State#state{logger = Pid};

create_logger(DevName, State = #state{dev = #device.name = DevName}) ->
   State;

create_logger(DevName, State) ->
   unlink(State#state.logger),
   mds_logger:stop(State#state.logger),
   create_logger(DevName, State#state{logger = undef}).

%=======================================================================================================================
% reply message to device
%=======================================================================================================================
reply(State, fuck_off, Socket) ->
   Msg = return_error(?ERROR_FUCK_OFF),
   log(State, debug, "Packet ~p was sent.", [Msg]),
   gen_tcp:send(Socket, Msg),
   self() ! stop;

reply(_State, noreply, _Socket) ->
   ok;

reply(State, Msg, Socket) ->
   log(State, debug, "Packet ~p was sent.", [Msg]),
   gen_tcp:send(Socket, Msg).

%=======================================================================================================================
% work with device status
%=======================================================================================================================
unregister(#state{dev = undef}) ->
   ok;
unregister(#state{db = Db, dev = #device{name = Name}}) ->
   gtracker_pub:unregister(Db, Name, ?MAX_CALL_TIMEOUT).

%=======================================================================================================================
% returns an error binary
%=======================================================================================================================
return_error(ErrNum) ->
   <<$D, ErrNum:8>>.

%=======================================================================================================================
% parse incoming packet and pass message to processing
%=======================================================================================================================
parsePacket(_, State = #state{socket = S, ecnt = ErrCnt}) when ErrCnt >= ?MAX_ERROR_COUNT ->
   log(State, warning, "Error count is exceeded. Device ~p will be fucked off.", [inet:peername(S)]),
   { fuck_off, State };

parsePacket(<<Type:?TYPE, Body/binary>>, State) ->
   processMsg(Type, Body, State);

parsePacket(Msg = <<>>, State = #state{ecnt = ErrCnt}) ->
   log(State, error, "Wrong message ~p.", [Msg]),
   {return_error(?ERROR_WRONG_MSG), State#state{ecnt = ErrCnt + 1}}.

%=======================================================================================================================
% processing of incoming messages
%=======================================================================================================================
% device requests new device name
processMsg(?AUTH_MSG, <<1:?VER>>, State = #state{socket = S, dev = undef, ref_prefix = RefPrefix}) ->
   {ok, PeerName} = inet:peername(S),
   log(State, info, "The device ~p requests a new device name.", [PeerName]),
   case gtracker_pub:register(State#state.db, ?MAX_CALL_TIMEOUT) of
      error ->
         {return_error(?ERROR_SERVER_UNAVAILABLE), State};
      Device = #device{name = DevName, reference = Ref} ->
         NewState = create_logger(DevName, State),
         log(State, info, "The device ~p got a device name ~p.", [PeerName, DevName]),
         BinDevName = erlang:list_to_bitstring(DevName),
         BinRef = fill_binary(erlang:list_to_binary(RefPrefix ++ Ref), ?REF_SIZE, <<0:8>>),
         { <<?AUTH_ACK_MSG, BinDevName:?BIN_DEV_NAME, BinRef/binary>>, NewState#state{dev = Device} }
   end;

processMsg(?AUTH_MSG, <<1:?VER>>, State) -> % the device already has a name
   BinDevName = erlang:list_to_bitstring(State#state.dev#device.name),
   BinRef = fill_binary(erlang:list_to_binary(State#state.ref_prefix ++ State#state.dev#device.reference), ?REF_SIZE, <<0:8>>),
   { <<?AUTH_ACK_MSG, BinDevName:?BIN_DEV_NAME, BinRef/binary>>, State };

% device has a assigned device name and want to auth with it
processMsg(?AUTH_MSG, <<1:?VER, BinDevName:?BIN_DEV_NAME>>,
   State = #state{socket = S, ref_prefix =RefPrefix, ecnt = ErrCnt}) ->
   DevName = erlang:bitstring_to_list(BinDevName),
   case gtracker_pub:register(State#state.db, DevName, ?MAX_CALL_TIMEOUT) of
      error ->
         {return_error(?ERROR_SERVER_UNAVAILABLE), State};
      no_such_device -> % wrong device name, hacker?
         log(State, warning, "Device name ~p not found. Error count ~p.", [DevName, ErrCnt + 1]),
         {return_error(?ERROR_WRONG_DEV_NAME), State#state{ecnt = ErrCnt + 1}};
      Device = #device{reference = Ref} ->
         log(State, info, "Device ~p was registered at ~p.", [DevName, inet:peername(S)]),
         NewState = create_logger(DevName, State),
         BinRef = fill_binary(erlang:list_to_binary(RefPrefix ++ Ref), ?REF_SIZE, <<0:8>>),
         {<<?AUTH_ACK_MSG, BinDevName:?BIN_DEV_NAME, BinRef/binary>>, NewState#state{dev = Device}};
      Msg ->
         log(State, error, "Unrecognized msg ~p during auth processing.", [Msg]),
         {return_error(?ERROR_SERVER_UNAVAILABLE), State}
   end;

% <<<<< BEGIN COORD processing >>>>>
processMsg(?COORD_MSG, _Msg, State = #state{socket = S, ecnt = ErrCnt, dev = undef}) ->
   log(State, error, "Device ~p sends coordinates, but not authenticated. Error count ~p",
     [inet:peername(S), ErrCnt + 1]),
   {return_error(?ERROR_NOT_AUTH), State#state{ecnt = ErrCnt + 1}};

processMsg(?COORD_MSG, Coord, State = #state{db = Db, dev = #device{name = DevName, subs = Subs}, track = undef, ecnt = ErrCnt}) ->
   case gtracker_pub:new_track(Db, DevName, false, ?MAX_CALL_TIMEOUT) of
      no_such_device ->
         {return_error(?ERROR_WRONG_DEV_NAME), State#state{ecnt = ErrCnt + 1}};
      Track when is_record(Track, track) ->
         gtracker_track_pub:set_subscribers(Track, Subs),
         processMsg(?COORD_MSG, Coord, State#state{track = Track})
   end;

% tries to store first coordinate after connect
processMsg(?COORD_MSG, <<Lat:?LAT, LatExp:?LAT_EXP, Lon:?LON, LonExp:?LON_EXP, Speed:?SPEED, TimeStamp:?TIMESTAMP>>, State =
   #state{track = Track, ccnt = 0}) ->
   {NewLat, NewLon, _, _, NewTimestamp} =
     {ints_to_float(Lat, LatExp), ints_to_float(Lon, LonExp), Speed, 0, unix_seconds_to_datetime(TimeStamp)},
   log(State, debug, "First coordinate received {~p, ~p, ~p}.", [NewLat, NewLon, NewTimestamp]),
   Coord = #coord{lat = NewLat, lon = NewLon, timestamp = NewTimestamp},
   gtracker_track_pub:store(Track, Coord),
   {noreply, State#state{ccnt = 1, last_coord = Coord}};

% tries to store next coordinates
processMsg(?COORD_MSG, <<Lat:?LAT, LatExp:?LAT_EXP, Lon:?LON, LonExp:?LON_EXP, Speed:?SPEED, TimeStamp:?TIMESTAMP>>, State =
   #state{track = Track, ccnt = Ccnt, last_coord = LastCoord, calc_speed = CalcSpeed}) ->
   {NewLat, NewLon, NewTimestamp} = {ints_to_float(Lat, LatExp), ints_to_float(Lon, LonExp), unix_seconds_to_datetime(TimeStamp)},
   log(State, debug, "Coordinate received {~p, ~p, ~p}.", [NewLat, NewLon, NewTimestamp]),
   #coord{lat = LastLat, lon = LastLon, timestamp = LastTimestamp} = LastCoord,
   Distance = nmea_utils:calc_distance({LastLat, LastLon}, {NewLat, NewLon}),
   SP = case CalcSpeed or ((Speed =:= 0) and (Distance =/= 0)) of
         true ->
            erlang:round(nmea_utils:calc_speed({LastLat, LastLon, LastTimestamp}, {NewLat, NewLon, NewTimestamp}));
         false ->
            Speed
   end,
   Coord = #coord{lat = NewLat, lon = NewLon, speed = SP, distance =  Distance, timestamp = NewTimestamp},
   gtracker_track_pub:store(Track, Coord),
   {noreply, State#state{ccnt = Ccnt + 1, last_coord = Coord}};

% <<<<< END COORD processing >>>>>

%processMsg(?RENAME_TRACK, <<_TrackName/bitstring>>, State = #state{socket = S, ecnt = ErrCnt, dev_name = undef}) ->
%   log(State, error, "Device ~p wants to rename track, but not authenticated. Error count ~p",
%      [inet:peername(S), ErrCnt + 1]),
%   {return_error(?ERROR_NOT_AUTH), State#state{ecnt = ErrCnt + 1}};

%processMsg(?RENAME_TRACK, <<BinTrackName/bitstring>>, State = #state{dev_name = DevName, ccnt = 0, ecnt = ErrCnt}) ->
%   TrackName = erlang:bitstring_to_list(BinTrackName),
%   log(State, info, "~p wants to rename current track to ~p, but track has not started yet", [DevName, TrackName]),
%   {return_error(?ERROR_TRACK_NOT_STARTED), State#state{ecnt = ErrCnt + 1}};

%processMsg(?RENAME_TRACK, <<BinTrackName/bitstring>>, State = #state{dev_name = DevName})
%when size(BinTrackName) =< 50 ->
%   TrackName = erlang:bitstring_to_list(BinTrackName),
%   log(State, info, "~p wants to rename current track to ~p", [DevName, TrackName]),
%   case rename_track(DevName, TrackName, State) of
%      error ->
%         {return_error(?ERROR_TRACK_RENAME), State};
%      ok ->
%         {<<?TRACK_STATUS, BinTrackName/binary>>, State}
%   end;

%processMsg(?RENAME_TRACK, <<_BinTrackName/bitstring>>, State) ->
%   {return_error(?ERROR_TRACK_NAME_TOO_LONG), State};

%processMsg(?START_NEW_TRACK, <<_TrackName/bitstring>>, State = #state{socket = S, ecnt = ErrCnt, dev_name = undef}) ->
%   log(State, error, "Device ~p wants to start new track, but not authenticated. Error count ~p",
%      [inet:peername(S), ErrCnt + 1]),
%   {return_error(?ERROR_NOT_AUTH), State#state{ecnt = ErrCnt + 1}};

%processMsg(?START_NEW_TRACK, <<BinTrackName/bitstring>>, State = #state{dev_name = DevName, ccnt = 0, ecnt = ErrCnt}) ->
%   TrackName = erlang:bitstring_to_list(BinTrackName),
%   log(State, info, "~p wants to start new track ~p, but there was no coordinates received.", [DevName, TrackName]),
%   {return_error(?ERROR_TRACK_NOT_STARTED), State#state{ecnt = ErrCnt + 1}};

%processMsg(?START_NEW_TRACK, <<BinTrackName/bitstring>>, State = #state{dev_name = DevName})
%when size(BinTrackName) =< 50 ->
%   TrackName = erlang:bitstring_to_list(BinTrackName),
%   log(State, info, "~p wants to start new track ~p", [DevName, TrackName]),
%   case start_new_track(DevName, TrackName, State) of
%      error ->
%         {return_error(?ERROR_START_NEW_TRACK), State};
%      ok ->
%         {<<?TRACK_STATUS, BinTrackName/binary>>, State}
%   end;

%processMsg(?START_NEW_TRACK, <<_BinTrackName/bitstring>>, State) ->
%   {return_error(?ERROR_TRACK_NAME_TOO_LONG), State};

%processMsg(?HEARTBEAT_MSG, <<>>, State) ->
%   log(State, debug, "Heartbeat message received."),
%   {<<?HEARTBEAT_RESPONSE>>, State};

%processMsg(?SOS_MSG, <<>>, #state{socket = S, dev_name = undef, ecnt = ErrCnt} = State) ->
%   log(State, debug, "SOS message received from device ~p, but device hasn't registered yet. Ignored. Error count ~p",
%      [inet:peername(S), ErrCnt + 1]),
%   {return_error(?ERROR_NOT_AUTH), State#state{ecnt = ErrCnt + 1}};

%processMsg(?SOS_MSG, <<>>, #state{dev_name = DevName, gt_pgroup = PGroup} = State) ->
%   log(State, debug, "SOS message received."),
%   send_pg(PGroup, {sos, DevName}),
%   {noreply, State};

processMsg(Type, Msg, State = #state{ecnt = ErrCnt}) ->
   log(State, error, "Wrong message ~p. Error count ~p.", [<<Type, Msg/binary>>, ErrCnt + 1]),
   {return_error(?ERROR_WRONG_MSG), State#state{ecnt = ErrCnt + 1}}.

%=======================================================================================================================
% log helpers
%=======================================================================================================================
log(State = #state{logger = undef}, LogLevel, Text) ->
   log2listener(State#state.listener, LogLevel, Text);

log(State, LogLevel, Text) ->
   mds_logger:log(State#state.logger, LogLevel, Text).

log(State = #state{logger = undef}, LogLevel, Format, Params) ->
   log2listener(State#state.listener, LogLevel, Format, Params);

log(State, LogLevel, Format, Params) ->
   mds_logger:log(State#state.logger, LogLevel, Format, Params).
