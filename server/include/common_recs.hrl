-record(email, {enabled = false, value = undef}).
-record(sms,   {enabled = false, value = undef}).
-record(device,
   {
      name, alias = undef, reference = undef, status = online, owner = undef, subs = [], timezone = "UTC",
      registered_at = now(), color = undef, weight = 1, pixmap = undef, twitter_auth = undef, current_track = undef,
      triggers = []
   }).
-record(user, {name, password, online = false, map_type = 0, is_admin = false,  devices = []}).
-record(track, {dev_name, id, name = undef, node = node()}).
-record(track_stat, {track_id, subs = [], status = opened, start = undef, stop = undef, length = 0, avg_speed = 0, coord_count = 0}).
-record(trigger, {dev_name, enabled, name, type, email, sms, twitter, text, config, ready = true, executed_at = undef, schedule = allways}).
-record(coord, {track_id, lat, lon, speed = 0, distance = 0, timestamp}).
-record(news, {id, date, text}).
