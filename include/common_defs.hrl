-define(DEF_GT_PGROUP, gtracker_pgroup).
-define(MSG(From, Group, Msg), {pg_message, From, Group, Msg}).

-record(trigger, {id, name, type, ready = true, email = undef, phone = undef, twitter = false, text = undef, config=undef,
      schedule=allways, executed_at = undef}).

-record(device, {id, name, alias, ref, online, tz, twitter_auth = undef}).

-define(FieldId(Rec, Field), string:str(record_info(fields, Rec), [Field]) + 1).
