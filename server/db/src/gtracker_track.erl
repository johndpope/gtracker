-module(gtracker_track).

-include("common_recs.hrl").
-include("common_defs.hrl").

-include_lib("eunit/include/eunit.hrl").

-export([open/4, init/4, loop/1]).

-record(state, {db, track_id, subs = [], ref, owner}).

open(Db, TrackId, Path, Owner) ->
   case erlang:whereis(TrackId) of
      undefined ->
         Pid = spawn_link(fun() -> init(Db, TrackId, Path, Owner) end),
         register(TrackId, Pid),
         Pid;
      Pid ->
         Pid
   end.

init(Db, TrackId, Path, Owner) ->
   process_flag(trap_exit, true),
   {ok, Ref} = dets:open_file(TrackId, ?track_open_args),
   loop(#state{db = Db, track_id = TrackId, ref = Ref, owner = Owner}).

loop(State = #state{db = Db, track_id = TrackId, subs = S, ref = Ref, owner = Owner}) ->
   receive
      Coord when is_record(Coord, coord) ->
         ok = dets:insert(Ref, Coord),
         NewS = gtracker_common:send2subs(S, Coord),
         loop(State#state{subs = NewS});
      {owner, NewOwner} ->
         error_logger:info_msg("The owner ~p has  been changed to ~p~n", [Owner, NewOwner]),
         link(NewOwner),
         loop(State#state{owner = NewOwner});
      {subscribers, NewSubs} ->
         loop(State#state{subs = NewSubs});
      clear ->
         dets:delete_all_objects(Ref),
         loop(State);
      {close, Peer} when Peer == Owner ->
         error_logger:info_msg("~p: owner ~p want me to close. Closing...~n", [TrackId, Owner]),
         dets:close(Ref),
         gen_server:cast(Db, #track_closed{track_id = TrackId});
      {close, _} ->
         loop(State);
      {'EXIT', Pid, Reason} when Pid =/= Owner ->
         error_logger:info_msg("~p: looks like old owner ~p exited with reason '~p'. Ignored.~n",
            [TrackId, Pid, Reason]),
         loop(State);
      {'EXIT', Owner, Reason} ->
         error_logger:info_msg("~p: owner ~p exited with reason '~p'. Closing...~n", [TrackId, Owner, Reason]),
         dets:close(Ref),
         gen_server:cast(Db, #track_closed{track_id = TrackId});
      Msg ->
         error_logger:error_msg("~p: Unknown message ~p was ignored~n", [TrackId, Msg]),
         loop(State)
   end.
