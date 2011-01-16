%-ifndef(COMMON_DEFS_HRL).
%-define(COMMON_DEFS_HRL, true).

-define(MSG(From, Group, Msg), {pg_message, From, Group, Msg}).
-define(FieldId(Rec, Field), string:str(record_info(fields, Rec), [Field]) + 1).


-define(db_ref, {global, gtracker_db}).
-define(track_ref, {local, gtracker_track}).

-define(MAX_CALL_TIMEOUT, 30000).
-define(track_open_args, [{auto_save, 1000}, {file, Path}, {keypos, ?FieldId(coord, timestamp)}]).

%-endif.
