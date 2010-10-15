-define(DEF_GT_PGROUP, gtracker_notif).
-define(MSG(From, Group, Msg), {pg_message, From, Group, Msg}).

-record(email, {enabled = false, value = undef}).

-record(sms,   {enabled = false, value = undef}).

-record(device, {id, name, alias = undef, reference = undef, online = false, registered_by = undef, timezone = "UTC", registered, color = undef, weight = 1,
      pixmap = undef, twitter_auth = undef, triggers = []}).

-record(user, {name, password}).

-record(track, {id, name, status = closed, filename = undef, start, stop, length, avg_speed}).

-record(trigger, {id, enabled, name, type, email, sms, twitter, text, config, ready = true, executed_at = undef, schedule = allways}).

-define(FieldId(Rec, Field), string:str(record_info(fields, Rec), [Field]) + 1).
