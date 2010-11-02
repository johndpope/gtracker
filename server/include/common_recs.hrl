-record(email, {enabled = false, value = undef}).
-record(sms,   {enabled = false, value = undef}).
-record(links, {owner = undef, track = undef, trigger = undef, web = undef}).
-record(device,
   {
      name, alias = undef, reference = undef, online = false, links = #links{}, timezone = "UTC",
      registered_at = now(), color = undef, weight = 1, pixmap = undef, twitter_auth = undef, current_track = undef
   }).
-record(user, {name, password, online = false, map_type = 0, is_admin = false,  devices = []}).
-record(track, {dev_name, node, id = erlang:make_ref(), name = undef, status = opened, path = undef, start = undef, stop = undef, length = 0, avg_speed = 0}).
-record(trigger, {dev_name, enabled, name, type, email, sms, twitter, text, config, ready = true, executed_at = undef, schedule = allways}).
-record(coord, {lat, lon, speed, timestamp}).
-record(news, {id, date, text}).
