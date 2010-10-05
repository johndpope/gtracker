var GTracker = {
        check: function(device, cb) {
                cb = $.extend({ success: $.noop, error: $.noop }, cb);
                $.getJSON(GTracker.path, { action: 'id', device: device }, function(json) {
                        if (json.error)
                                cb.error(json.data);
                        else
                                cb.success(json.data);
                });
        },

        settings: function(id, cb) {
                cb = $.extend({ success: $.noop, error: $.noop }, cb);
                $.getJSON(GTracker.path, { action: 'get_opts', id: id }, function(json) {
                        if (json.error) {
                                cb.error(json.data);
                        } else {
                                $.extend(json.data, { image: 'http://gtracker.ru/img/' + id });
                                cb.success(json.data);                        
                        }
                });
        },

        Link: function(id, cb) {
                var $timer = null;
                var $last_location_id = null;
                var $ref = this;

                cb = $.extend({ location_updated: $.noop, state_changed: $.noop, speed_changed: $.noop }, cb);

                var check_location = function() {
                        var opts = { action: 'get_loc', id: id };
                        if ($last_location_id) {
                                opts.last_id = $last_location_id;
                        }

                        $.getJSON(GTracker.path, opts, function(json) {
                                if (!json.error && json.size > 0) {
                                        $last_location_id = json.data.pop();
                                        cb.location_updated(json.data, json.size - 1);
                                }
                        });
                };

                var check_state = function() {
                        $.getJSON(GTracker.path, { action: 'online', id: id }, function(json) {
                                if (!json.error && json.data.online != $ref.online) {
                                        $ref.online = json.data.online;
                                        cb.state_changed($ref.online);
                                }

                                if ($ref.online && $ref.update_location) {
                                        if (json.data.speed != null) {
                                                cb.speed_changed(json.data.speed);
                                        }
                                        check_location();
                                }
                        });
                };

                $.extend($ref, {
                        update_location: false,
                        online: false,

                        init: function() {
                                if ($timer) {
                                        clearInterval($timer);
                                        $timer = null;
                                }

                                $ref.update_location = false;
                                $ref.online = false;

                                check_state();
                                $timer = setInterval(check_state, GTracker.check_interval);
                        },

                        destroy: function() {
                                if ($timer) {
                                        clearInterval($timer);
                                        $timer = null;
                                }
                        },

                        reset_position: function() {
                                $last_location_id = null;
                        },

                        tracks: function(cb) {
                                cb = $.extend({ success: $.noop, error: $.noop }, cb);
                                $.getJSON(GTracker.path, { action: 'tracks', id: id }, function(json) {
                                        if (json.error)
                                                cb.error(json.data);
                                        else
                                                cb.success(json.data, json.size);
                                });
                        },

                        get_track: function(track_id, cb) {
                                cb = $.extend({ success: $.noop, error: $.noop }, cb);
                                $.getJSON(GTracker.path, { action: 'get_track', track_id: track_id, id: id }, function(json) {
                                        if (json.error)
                                                cb.error(json.data);
                                        else
                                                cb.success(json.data, json.size);
                                });
                        },

                        remove_track: function(track_id, cb) {
                                cb = $.extend({ success: $.noop, error: $.noop }, cb);
                                $.getJSON(GTracker.path, { action: 'remove_track', track_id: track_id, id: id }, function(json) {
                                        if (json.error)
                                                cb.error(json.data);
                                        else
                                                cb.success(json.data);
                                });
                        },

                        rename_track: function(track_id, name, cb) {
                                cb = $.extend({ success: $.noop, error: $.noop }, cb);
                                $.getJSON(GTracker.path, { action: 'rename_track', track_id: track_id, name: name, id: id }, function(json) {
                                        if (json.error)
                                                cb.error(json.data);
                                        else
                                                cb.success(json.data);
                                });
                        },

                        make_track_link: function(track_id, cb) {
                                cb = $.extend({ success: $.noop, error: $.noop }, cb);
                                $.getJSON(GTracker.path, { action: 'make_track_link', track_id: track_id, id: id }, function(json) {
                                        if (json.error)
                                                cb.error(json.data);
                                        else
                                                cb.success(json.data);
                                });
                        },

                        get_config: function(cb) {
                                cb = $.extend({ success: $.noop, error: $.noop }, cb);
                                $.getJSON(GTracker.path, { action: 'get_config', id: id }, function(json) {
                                        if (json.error) {
                                                cb.error(json.data);
                                        } else {
                                                cb.success(json.data);                        
                                        }
                                });
                        },

                        set_config: function(config, cb) {
                                cb = $.extend({ success: $.noop, error: $.noop }, cb);
                                $.getJSON(GTracker.path, {
                                                action: 'set_config',
                                                id: id,
                                                alias: config.alias,
                                                timezone: config.timezone,
                                                color: config.color,
                                                weight: config.weight
                                          },
                                          function(json) {
                                                  if (json.error) {
                                                          cb.error(json.data);
                                                  } else {
                                                          cb.success(json.data);                        
                                                  }
                                          });
                        }
                });
        },

        load_public_track: function(public_id, cb) {
                cb = $.extend({ success: $.noop, error: $.noop}, cb);
                $.getJSON(GTracker.path, { action: 'get_public_track', link: public_id }, function(json) {
                        if (json.error)
                                cb.error(json.data);
                        else
                                cb.success(json.data, json.size);
                });
        },

        show_public_device: function(public_id, cb) {
                cb = $.extend({ location_updated: $.noop, state_changed: $.noop, error: $.noop }, cb);
                var $timer = null;
                var $last_location_id = null;
                var $ref = this;

                //init 
        },

        path: 'http://gtracker.ru/api',
        check_interval: 5000
};
