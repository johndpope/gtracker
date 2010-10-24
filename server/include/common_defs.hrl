-define(DEF_GT_PGROUP, gtracker_notif).
-define(MSG(From, Group, Msg), {pg_message, From, Group, Msg}).

-record(email, {enabled = false, value = undef}).

-record(sms,   {enabled = false, value = undef}).

-record(links, {owner = undef, track = undef, trigger = undef, web = undef}).

-record(device,
   {
      name, alias = undef, reference = undef, online = false, links = #links{}, timezone = "UTC",
      registered_at = now(), color = undef, weight = 1, pixmap = undef, twitter_auth = undef
   }).

-record(user, {name, password, online = false, is_admin = false}).

-record(track, {dev_name, id = erlang:make_ref(), name = undef, status = closed, path = undef, start = undef, stop = undef, length = 0, avg_speed = 0}).

-record(trigger, {dev_name, enabled, name, type, email, sms, twitter, text, config, ready = true, executed_at = undef, schedule = allways}).

-record(coord, {lat, lon, speed, timestamp}).

-define(FieldId(Rec, Field), string:str(record_info(fields, Rec), [Field]) + 1).
