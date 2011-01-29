-module(test_device).

-compile([export_all]).

-record(state, {socket = undef, did = undef}).

-include("../../../include/fields.hrl").

stop(Name) ->
   Name ! stop.

emulate(Name, CommandsFile) ->
   {ok, Commands} = file:consult(CommandsFile),
   Pid = spawn_link(fun() -> emul_init(Commands) end),
   register(Name, Pid).

mass_start(_CommandsFile, 0, _, _) ->
   ok;
mass_start(CommandsFile, ClientCount, Portion, Timeout) ->
   if (ClientCount rem Portion == 0) -> timer:sleep(Timeout); true -> ok end,
   emulate(list_to_atom("ddd" ++ integer_to_list(ClientCount)), CommandsFile),
   mass_start(CommandsFile, ClientCount - 1, Portion, Timeout).

emul_init(Commands) ->
   emul_loop(Commands, #state{}).

emul_loop([], #state{socket = Socket}) ->
   gen_tcp:close(Socket),
   io:format("Finished~n");

emul_loop([{connect, Host, Port}|Rest], State) ->
   {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 1}, {send_timeout, 30000}]),
   receive
      {tcp_closed, _Socket} ->
         io:format("connection closed by server~n"),
         ok;
      {tcp, _Socket, <<$K:8, BinRHost:?HOST, RPort:?PORT>>} ->
         RHost = truncate(BinRHost, []),
         io:format("Redirected to ~p:~p~n", [RHost, RPort]),
         gen_tcp:close(Socket),
         emul_loop([{connect, RHost, RPort}|Rest], State);
      {tcp, _Socket, Data} ->
         io:format("Data received from server: ~p~n", [Data]),
         emul_loop(Rest, State#state{socket = Socket})
   after 50 ->
      emul_loop(Rest, State#state{socket = Socket})
   end;
emul_loop([auth|Rest], #state{socket = Socket, did = undef} = State) ->
   gen_tcp:send(Socket, <<$A, 1:8>>),
   io:format("Auth was sent~n"),
   wait_auth(Rest, State);
emul_loop([auth|Rest], #state{socket = Socket, did = DID} = State) ->
   BinDID = list_to_binary(DID),
   gen_tcp:send(Socket, <<$A, 1:8, BinDID/binary>>),
   io:format("Auth ~p was sent~n", [DID]),
   wait_auth(Rest, State);
emul_loop([{auth, DID}|Rest], #state{socket = Socket} = State) ->
   BinDID = list_to_binary(DID),
   gen_tcp:send(Socket, <<$A, 1:8, BinDID/binary>>),
   io:format("Auth ~p was sent~n", [DID]),
   wait_auth(Rest, State);
emul_loop([{auth, reset}|Rest], #state{socket = Socket} = State) ->
   gen_tcp:send(Socket, <<$A, 1:8>>),
   io:format("Auth was sent~n"),
   wait_auth(Rest, State);
emul_loop([stop|Rest], #state{socket = Socket} = State) ->
   gen_tcp:close(Socket),
   io:format("Closing...~n"),
   emul_loop(Rest, State);
emul_loop([{wait, Timeout}|Rest], #state{socket = Socket} = State) ->
   io:format("Waiting for ~p ms...~n", [Timeout]),
   timer:sleep(Timeout),
   receive
      {tcp_closed, Socket} ->
         io:format("Connection closed by server~n");
      {tcp, Socket, Data} ->
         io:format("Data received from server: ~p~n", [Data]),
         emul_loop(Rest, State)
   after 0 ->
      emul_loop(Rest, State)
   end;
emul_loop([{coord, Lat, Lon, Speed, Timestamp} = Coord|Rest], #state{socket = Socket} = State) ->
   LatMan = trunc(Lat),
   LatExp = round((Lat - LatMan) * 1000000),
   LonMan = trunc(Lon),
   LonExp = round((Lon - LonMan) * 1000000),
   Tm = str2tm(Timestamp),
   gen_tcp:send(
      Socket,
      <<$C:?TYPE, LatMan:?LAT, LatExp:?LAT_EXP, LonMan:?LON, LonExp:?LON_EXP, Speed:?SPEED, Tm:?TIMESTAMP>>),
   io:format("~p~n", [Coord]),
   receive
      {tcp_closed, Socket} ->
         io:format("Connection closed by server~n");
      {tcp, Socket, Data} ->
         io:format("Data received from server: ~p~n", [Data]),
         emul_loop(Rest, State);
      stop ->
         gen_tcp:close(Socket),
         io:format("Stopped~n")
   after 1000 ->
      emul_loop(Rest, State)
   end;
emul_loop([heartbeat|Rest], #state{socket = Socket} = State) ->
   gen_tcp:send(Socket, <<$E>>),
   io:format("Heartbeat was sent~n"),
   receive
      {tcp_closed, Socket} ->
         io:format("Connection closed by server~n");
      {tcp, Socket, Data} ->
         io:format("Data received from server: ~p~n", [Data]),
         emul_loop(Rest, State);
      stop ->
         gen_tcp:close(Socket),
         io:format("Stopped~n")
   after 0 ->
      emul_loop(Rest, State)
   end;
emul_loop([{send, Binary}|Rest], #state{socket = Socket} = State) ->
   gen_tcp:send(Socket, Binary),
   io:format("Binary data was sent: ~p~n", [Binary]),
   receive
      {tcp_closed, Socket} ->
         io:format("Connection closed by server~n");
      {tcp, Socket, Data} ->
         io:format("Data received from server: ~p~n", [Data]),
         emul_loop(Rest, State);
      stop ->
         gen_tcp:close(Socket),
         io:format("Stopped~n")
   after 0 ->
      emul_loop(Rest, State)
   end;
emul_loop([Msg = {rename_track, TrackName}|Rest], #state{socket = Socket} = State) ->
   BinTrackName = erlang:list_to_binary(TrackName),
   gen_tcp:send(Socket, <<$H, BinTrackName/binary>>),
   io:format("~p~n", [Msg]),
   receive
     {tcp_closed, Socket} ->
        io:format("Connection closed by server~n");
     {tcp, Socket, Data} ->
        io:format("Data received from server: ~p~n", [Data]),
        emul_loop(Rest, State);
     stop ->
        gen_tcp:close(Socket),
        io:format("Stopped~n")
   after 0 ->
     emul_loop(Rest, State)
   end;
emul_loop([Msg = {start_new_track, TrackName}|Rest], #state{socket = Socket} = State) ->
   BinTrackName = erlang:list_to_binary(TrackName),
   gen_tcp:send(Socket, <<$G, BinTrackName/binary>>),
   io:format("~p~n", [Msg]),
   receive
      {tcp_closed, Socket} ->
         io:format("Connection closed by server~n");
      {tcp, Socket, Data} ->
         io:format("Data received from server: ~p~n", [Data]),
         emul_loop(Rest, State);
      stop ->
         gen_tcp:close(Socket),
         io:format("Stopped~n")
   after 0 ->
      emul_loop(Rest, State)
   end;
emul_loop([Command|Rest], State) ->
   io:format("Unknown command <~p>. Skipped.", [Command]),
   emul_loop(Rest, State).

wait_auth(Commands, #state{socket = Socket} = State) ->
   receive
      {tcp_closed, Socket} ->
         io:format("Connection closed by server~n");
      {tcp, _Socket, <<$B:8, BinDID:?BIN_DEV_NAME, BinRef/bitstring>>} ->
         DID = erlang:bitstring_to_list(BinDID),
         Ref = erlang:bitstring_to_list(BinRef),
         io:format("Authenticated as ~p. Ref = ~p.~n", [DID, Ref]),
         emul_loop(Commands, State#state{did = DID});
      {tcp, _Socket, Data} ->
         io:format("Data received from server: ~p~n", [Data]),
         emul_loop(Commands, State);
      stop ->
         gen_tcp:close(Socket),
         io:format("Stopped~n")
   after 1000 ->
      io:format("No responce from server~n"),
      ok
   end.

str2tm(StrDate) ->
   [YYYY, M, DD, HH, MM, SS] = string:tokens(StrDate, ": -"),
   Date = {{list_to_integer(YYYY), list_to_integer(M), list_to_integer(DD)},{list_to_integer(HH), list_to_integer(MM),
         list_to_integer(SS)}},
   calendar:datetime_to_gregorian_seconds(Date) - calendar:datetime_to_gregorian_seconds({{1970,1,1},{00,00,00}}).

truncate(<<0:8, _Rest/binary>>, List) ->
   lists:reverse(List);
truncate(<<A:8, Rest/binary>>, List) ->
   truncate(Rest, [A|List]).
