-module(gtracker_track).

-include("common_recs.hrl").

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
   {ok, Ref} = dets:open_file(TrackId, [{auto_save, 1000}, {file, Path}, {keypos, 5}]),
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
         error_logger:info_msg("~p: owner ~p exited. Closing...~n", [TrackId, Owner]),
         dets:close(Ref);
      {close, _} ->
         loop(State);
      {'EXIT', _, _} ->
         error_logger:info_msg("~p: Owner ~p exited. Closing...~n", [TrackId, Owner]),
         dets:close(Ref);
      Msg ->
         error_logger:error_msg("~p: Unknown message ~p was ignored~n", [TrackId, Msg]),
         loop(State)
   end.
