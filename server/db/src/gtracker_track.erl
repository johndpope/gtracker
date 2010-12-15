-module(gtracker_track).

-include("common_recs.hrl").
-include("common_defs.hrl").

-include_lib("eunit/include/eunit.hrl").

-export([open/3, init/3, loop/1]).

-record(state, {track_id, ref, owner}).

open(TrackId, Path, Owner) ->
   case erlang:whereis(TrackId) of
      undefined ->
         Pid = spawn_link(fun() -> init(TrackId, Path, Owner) end),
         register(TrackId, Pid),
         Pid;
      Pid ->
         Pid
   end.

init(TrackId, Path, Owner) ->
   {ok, Ref} = dets:open_file(TrackId, [{auto_save, 1000}, {file, Path}, {keypos, ?FieldId(coord, timestamp)}]),
   process_flag(trap_exit, true),
   loop(#state{track_id = TrackId, ref = Ref, owner = Owner}).

loop(State = #state{track_id = TrackId, ref = Ref, owner = Owner}) ->
   receive
      Coord when is_record(Coord, coord) ->
         ok = dets:insert(Ref, Coord),
         loop(State);
      {get_coords, Peer} ->
         Coords = dets:select(Ref, [{'_', [], ['$_']}]),
         Peer ! {reply, Coords},
         loop(State);
      clear ->
         dets:delete_all_objects(Ref),
         loop(State);
      {close, Peer} when Peer == Owner ->
         error_logger:info_msg("~p: owner ~p want me to close. Closing...~n", [TrackId, Owner]),
         dets:close(Ref),
         gen_server:cast(?db_ref, #track_closed{track_id = TrackId});
      {close, _} ->
         loop(State);
      {'EXIT', _, _} ->
         error_logger:info_msg("~p: owner ~p exited. Closing...~n", [TrackId, Owner]),
         dets:close(Ref),
         gen_server:cast(?db_ref, #track_closed{track_id = TrackId});
      Msg ->
         error_logger:error_msg("~p: Unknown message ~p was ignored~n", [TrackId, Msg]),
         loop(State)
   end.
