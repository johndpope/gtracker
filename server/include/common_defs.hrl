%-ifndef(COMMON_DEFS_HRL).
%-define(COMMON_DEFS_HRL, true).

-define(MSG(From, Group, Msg), {pg_message, From, Group, Msg}).
-define(FieldId(Rec, Field), string:str(record_info(fields, Rec), [Field]) + 1).


-define(db_ref, {global, gtracker_db}).
-define(track_ref, {local, gtracker_track}).

-define(MAX_CALL_TIMEOUT, 30000).
-define(track_open_args, [{auto_save, 1000}, {file, Path}, {keypos, ?FieldId(coord, timestamp)}]).

-define(def_metric_send_period, 1000). % in ms -> one second
-define(metric_collector, gt_metrics).

-define(coord_rate, coord_rate).
-define(message_queue_len, message_queue_len).
-define(memory, memory).
-define(cpu, cpu).
-define(online_devices, online_devices).
-define(active_tracks, active_tracks).
-define(active_clients, active_clients).

%-endif.
