var DeviceHandle = {
        make_template: function() {
                return $('<div class="handle">' +
                         '      <div class="image"/>' +
                         '      <ul class="info">' +
                         '              <li><div class="name">' + $.tr('device') + '</div><div class="device"></div></li>' +
                         '              <li><div class="name">' + $.tr('status') + '</div><div class="status"></div></li>' +
                         '              <li><div class="name">' + $.tr('speed') + '</div><div class="speed"></div></li>' +
                         '      </ul>' +
                         '      <ul class="menu">' +
                         '              <li class="begin"></li>' +
                         '              <li class="end"></li>' +
                         '              <li class="archive"></li>' +
                         '              <li class="close"></li>' +
                         '      </ul>' +
                         '      <ul class="list">' +
                         '      </ul>' +
                         '</div>');
        },

        make_track_template: function() {
                return $('<li>' +
                         '      <div class="info">' +
                         '              <div class="name"/>' +
                         '              <div class="start"/>' +
                         '              <div class="stop"/>' +
                         '      </div>' +
                         '      <div class="control">' +
                         '              <div class="make_link">' + $.tr('make_link') + '</div>' +
                         '              <div class="rename">' + $.tr('rename') + '</div>' +
                         '              <div class="remove">' + $.tr('remove') + '</div>' +
                         '      </div>' +
                         '</li>');
        },

        make_config_template: function(json) {
                var $timezone_opts = '<select name="timezone">' +
                        '<option value="Pacific/Midway" >(GMT -11:00 hours) Midway Island, Samoa</option>' +
                        '<option value="America/Adak" >(GMT -10:00 hours) Hawaii</option>' +
                        '<option value="America/Anchorage" >(GMT -9:00 hours) Alaska</option>' +
                        '<option value="America/Dawson" >(GMT -8:00 hours) Pacific Time (US & Canada)</option>' +
                        '<option value="America/Inuvik" >(GMT -7:00 hours) Mountain Time (US & Canada)</option>' +
                        '<option value="America/Winnipeg" >(GMT -6:00 hours) Central Time (US & Canada), Mexico City</option>' +
                        '<option value="America/Louisville" >(GMT -5:00 hours) Eastern Time (US & Canada), Bogota, Lima, Quito</option>' +
                        '<option value="America/Caracas" >(GMT -4:00 hours) Atlantic Time (Canada), Caracas, La Paz</option>' +
                        '<option value="America/St_Johns" >(GMT -3:30 hours) Newfoundland</option>' +
                        '<option value="America/Buenos_Aires" >(GMT -3:00 hours) Brazil, Buenos Aires, Georgetown</option>' +
                        '<option value="America/Noronha" >(GMT -2:00 hours) Mid-Atlantic</option>' +
                        '<option value="Atlantic/Azores" >(GMT -1:00 hours) Azores, Cape Verde Islands</option>' +
                        '<option value="Europe/London" >(GMT) Western Europe Time, London, Lisbon, Casablanca, Monrovia</option>' +
                        '<option value="Europe/Paris" >(GMT +1:00 hours) CET(Central Europe Time), Brussels, Copenhagen, Madrid, Paris</option>' +
                        '<option value="Europe/Kaliningrad" >(GMT +2:00 hours) EET(Eastern Europe Time), Kaliningrad, South Africa</option>' +
                        '<option value="Europe/Moscow" >(GMT +3:00 hours) Baghdad, Kuwait, Riyadh, Moscow, St. Petersburg, Volgograd, Nairobi</option>' +
                        '<option value="Asia/Tehran" >(GMT +3:30 hours) Tehran</option>' +
                        '<option value="Asia/Muscat" >(GMT +4:00 hours) Abu Dhabi, Muscat, Baku, Tbilisi</option>' +
                        '<option value="Asia/Kabul" >(GMT +4:30 hours) Kabul</option>' +
                        '<option value="Asia/Karachi" >(GMT +5:00 hours) Ekaterinburg, Islamabad, Karachi, Tashkent</option>' +
                        '<option value="Asia/Calcutta" >(GMT +5:30 hours) Bombay, Calcutta, Madras, New Delhi</option>' +
                        '<option value="Asia/Dhaka" >(GMT +6:00 hours) Almaty, Dhaka, Colombo</option>' +
                        '<option value="Asia/Jakarta" >(GMT +7:00 hours) Bangkok, Hanoi, Jakarta</option>' +
                        '<option value="Asia/Singapore" >(GMT +8:00 hours) Beijing, Perth, Singapore, Hong Kong, Chongqing, Urumqi, Taipei</option>' +
                        '<option value="Asia/Tokyo" >(GMT +9:00 hours) Tokyo, Seoul, Osaka, Sapporo, Yakutsk</option>' +
                        '<option value="Australia/Darwin" >(GMT +9:30 hours) Adelaide, Darwin</option>' +
                        '<option value="Asia/Vladivostok" >(GMT +10:00 hours) EAST(East Australian Standard), Guam, Papua New Guinea, Vladivostok</option>' +
                        '<option value="Asia/Magadan" >(GMT +11:00 hours) Magadan, Solomon Islands, New Caledonia</option>' +
                        '<option value="Asia/Kamchatka" >(GMT +12:00 hours) Auckland, Wellington, Fiji, Kamchatka, Marshall Island</option>' +
                '</select>';

                return $.tr('alias') + ':<br/><input name="alias" style="width: 100%" value="' + json.alias + '"/><br/>' +
                       $.tr('timezone') + ': <br/>' + $timezone_opts.replace('value="' + json.timezone + '"', 'value="' + json.timezone + '" selected') + '<br/>' +
                       $.tr('line_color') + ': <br/><input name="color" style="width: 100%" value="' + json.color + '"/><br/>' +
                       $.tr('line_weight') + ': <br/><input name="weight" style="width: 100%" value="' + json.weight + '"/><br/>';
//                       $.tr('twitter_key') + ':<br/><input name="twitter_key" style="width: 100%"/><br/>' +
//                       $.tr('twitter_secret') + ':<br/><input name="twitter_secret" style="width: 100%"/>';
        },

        make: function(info, map) {
                var $handle = DeviceHandle.make_template();

                $.extend($handle,
                         {
                                 link: new GTracker.Link(info.id, {
                                         location_updated: function(points, size) {
                                                 $handle.track.append(points, size);
                                         },
                                         state_changed: function(state) {
                                                 $handle.find('.info .status').text($.tr(state ? 'online' : 'offline'));
                                                 if (!state) {
                                                         $handle.find('.info .speed').text('0 ' + $.tr('kmh'));
                                                 }
                                                 $handle.track.clear();
                                                 $handle.link.reset_position();
                                         },
                                         speed_changed: function(speed) {
                                                $handle.find('.info .speed').text(speed + ' ' + $.tr('kmh'));
                                         }
                                 }),

                                 track: new GMap.Track(map),

                                 disable: function() {
                                         $handle.track.setVisible(false);
                                         $handle.slideUp('slow');
                                 },

                                 enable: function() {
                                         $handle.track.setVisible(true);
                                         $handle.slideDown('slow');
                                 },

                                 destroy: function() {
                                         $handle.link.destroy();
                                         $handle.track.setVisible(false);
                                         $handle.track.clear(); // add destroy();
                                         $handle.remove();
                                 },

                                 device: info.device
                         });

                $handle.find('.info .device').text(info.device);
                $handle.find('.info .status').text($.tr('offline'));
                $handle.find('.info .speed').text('0 ' + $.tr('kmh'));
                $handle.find('.menu .begin').text($.tr('begin')).click(function() {
                        $handle.track.showFirst();
                });
                $handle.find('.menu .end').text($.tr('end')).click(function() {
                        $handle.track.showLast();
                }).dblclick(function() {
                        $handle.track.autoPan = !$handle.track.autoPan;
                        if ($handle.track.autoPan)
                                $(this).addClass('select');
                        else
                                $(this).removeClass('select');
                });
                $handle.find('.menu .archive').text($.tr('archive')).click(function() {
                        var $track_list = $handle.find('.list');
                        $handle.link.update_location = !$handle.link.update_location;
                        $handle.track.clear();
                        if ($handle.link.update_location) {
                                $(this).removeClass('select');
                                $track_list.slideUp('up');
                                $handle.trigger('archive');
                                $handle.link.reset_position();
                        } else {
                                $(this).addClass('select');
                                $track_list.children('li').remove();
                                $track_list.append('<li class="msg">' + $.tr('loading') + '</li>');
                                $track_list.slideDown('slow');
                                $handle.link.tracks({
                                        success: function(tracks) {
                                                $track_list.children('li').remove();
                                                if (tracks.length > 0 ) {
                                                        $.each(tracks, function()  {
                                                                var $track = DeviceHandle.make_track_template();
                                                                var $id = this.id;
                                                                $track.find('.start').text(this.start);
                                                                $track.find('.stop').text(this.stop);
                                                                $track.find('.name').text(this.name).attr('title', $.tr('interval') + ': \n' + this.start + ' - ' + this.stop);

                                                                if (this.name != '') {
                                                                        $track.find('.start').hide();
                                                                        $track.find('.stop').hide();
                                                                } else {
                                                                        $track.find('.name').hide();
                                                                }

                                                                $track.find('.info').click(function() {
                                                                        $handle.link.get_track($id, {
                                                                                success: function(points, count) {
                                                                                        $handle.track.clear();
                                                                                        $handle.track.append(points, count);
                                                                                        $handle.track.showFirst();
                                                                                },

                                                                                error: function() {
                                                                                        Notify.show_error('Track load failed');
                                                                                }
                                                                        });
                                                                });

                                                                $track.find('.remove').click(function() {
                                                                        $.prompt($.tr('confirm_remove'), {
                                                                                callback: function(remove) {
                                                                                        if (remove) {
                                                                                                $handle.link.remove_track($id, {
                                                                                                        success: function(result) {
                                                                                                                if (result) {
                                                                                                                        $track.remove();
                                                                                                                        $handle.track.clear();
                                                                                                                }
                                                                                                        },
                                                                                                });
                                                                                        }
                                                                                }
                                                                        });
                                                                });

                                                                $track.find('.make_link').click(function() {
                                                                        $handle.link.make_track_link($id, {
                                                                                success: function(link) {
                                                                                        var $prompt = $.tr('your_link') + ': <br/><input type="text" value="' + link + '" readonly style="width: 400px"/>';
                                                                                        var $buttons = {};
                                                                                        $.prompt($prompt, { buttons: { OK: true } });
                                                                                },

                                                                                error: function() {
                                                                                        Notify.show_error('Make link failed');
                                                                                }
                                                                        });
                                                                });

                                                                $track.find('.rename').click(function() {
                                                                        var $prompt = $.tr('new_name') + ': <br/><input type="text" id="track_name" name="track_name" value="" maxlength="50" style="width: 400px"/>';
                                                                        $.prompt($prompt, {
                                                                                callback: function(rename, title, args) {
                                                                                        if (rename) {
                                                                                                $handle.link.rename_track($id, args.track_name, {
                                                                                                        success: function(result) {
                                                                                                                if (result) {
                                                                                                                        if (args.track_name != '') {
                                                                                                                                $track.find('.name').text(args.track_name).show();
                                                                                                                                $track.find('.start').hide();
                                                                                                                                $track.find('.stop').hide();
                                                                                                                        } else {
                                                                                                                                $track.find('.name').hide();
                                                                                                                                $track.find('.start').show();
                                                                                                                                $track.find('.stop').show();
                                                                                                                        }
                                                                                                                }
                                                                                                        }
                                                                                                });
                                                                                        }
                                                                                }
                                                                        });
                                                                });

                                                                $track_list.append($track);
                                                        });
                                                } else {
                                                        $track_list.append('<li class="msg">' + $.tr('no_tracks') + '</li>');
                                                }
                                        },

                                        error: function() {
                                                $track_list.remove('li');
                                                $track_list.append($('<li class="msg">' + $.tr('track_list_error') + '</li>'));
                                        }
                                });
                                $handle.trigger('online');
                        }
                });
                $handle.find('.image').click(function() {
                        $handle.link.get_config({
                                success: function(data) {
                                        $.prompt(DeviceHandle.make_config_template(data), {
                                                callback: function(result, skip, args) {
                                                        if (result) {
                                                                $handle.link.set_config(args);
                                                                $handle.track.setOptions(args);
                                                        }
                                                }
                                        });
                                }
                        });
                });

                $handle.find('.menu .close').text($.tr('close')).click(function() {
                        $handle.trigger('close');
                });
                $handle.find('.list').hide();

                $handle.track.setVisible(true);
                $handle.link.init();
                $handle.link.update_location = true;

                GTracker.settings(info.id, {
                        success: function(settings) {
                                $handle.track.setOptions(settings);
                                $handle.find('.image').css('background', 'url(' + settings.image + ')');
                        }
                });

                return $handle;
        }
};

var Notify = {
        show_warning: function(msg) {
                alert('WRN: ' + msg);
        },

        show_error: function(msg) {
                alert('ERR: ' + msg);
        },

        show_info: function(msg) {
                //alert('MSG: ' + msg);
        }
};

var Cookie = {
        expires: 3,
        delimeter: ';',

        insert: function(name, value) {
                var $values = Cookie.values(name);
                if ($values.indexOf(value) == -1) {
                        $values.push(value);
                        $.cookie(name, $values.join(Cookie.delimeter), { path: '/', expires: Cookie.expires });
                }
        },

        remove: function(name, value) {
                var $values = Cookie.values(name);
                var $index = $values.indexOf(value);
                if ($index != -1) {
                        $values.splice($index, 1);
                }

                if ($values.length > 0) {
                        $.cookie(name, $values.join(Cookie.delimeter), { path: '/', expires: Cookie.expires });
                } else {
                        $.cookie(name, null);
                }
        },

        values: function(name) {
                var $values = $.cookie(name) || '';
                return $values.length > 0 ? $values.split(Cookie.delimeter) : [];
        }
};

var Page = {
        initMap: function(parent) {
                var $map = new GMap.Map(parent);
                $map.setCenter(55.755206, 37.623281);
                $map.setZoom(12);
                $map.setType('roadmap');
                return $map;
        },

        initEffects: function(parent) {
                parent.find('.sidebar .input').blurNotify($.tr('device'));
        },

        initPrompt: function() {
                var $buttons = {};
                $buttons[$.tr('ok')] = true;
                $buttons[$.tr('cancel')] = false;
                $.prompt.setDefaults({ buttons: $buttons, focus: 1 });
        },

        init: function(parent) {
                var $map = Page.initMap(parent.find('.map'));
                var $sidebar = parent.find('.sidebar');
                var $append = parent.find('.append');
                var $handles = [];

                var $check_cb = {
                        success: function(info) {
                                for (var i in $handles) {
                                        if ($handles[i].device == info.device) {
                                                Notify.show_warning($.tr('already_exists'));
                                                return ;
                                        }
                                }

                                Cookie.insert('DEVICES', info.device);
                                var $handle = DeviceHandle.make(info, $map);
                                $handle.bind({
                                        close: function() {
                                                Cookie.remove('DEVICES', info.device);
                                                var $index = $handles.indexOf($handle);
                                                if ($index != -1) {
                                                        $handles.splice($index, 1);
                                                }
                                                $handle.destroy();
                                        },
                                        online: function() {
                                                $.each($handles, function() {
                                                        if ($handle != this) {
                                                                this.disable();
                                                        }
                                                });
                                                $append.slideUp('slow');
                                        },
                                        archive: function() {
                                                $.each($handles, function() {
                                                        if ($handle != this) {
                                                                this.enable();
                                                        }
                                                });
                                                $append.slideDown('slow');
                                        }

                                });
                                $handle.hide();
                                $handle.appendTo($sidebar);
                                $handle.slideDown('slow');
                                $handles.push($handle);
                        },

                        error: function(msg) {
                                Notify.show_error(msg);
                        }
                };

                parent.find('form').submit(function() {
                        var $input = $(this).find('.input');
                        GTracker.check($input.val(), $check_cb);
                        $input.val('').blur();
                        return false;
                });

                Page.initEffects(parent);
                Page.initPrompt();

                $.each(Cookie.values('DEVICES'), function() {
                        GTracker.check(this.toString(), $check_cb);
                });
        }
};
