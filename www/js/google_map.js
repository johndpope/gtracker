var GMap = {
        type: 'Google',

        Map: function(element) {
                var $map = new google.maps.Map(element[0], { disableDefaultUI: true });

                $.extend(this, {
                        setCenter: function(lat, lon) {
                                $map.setCenter(new google.maps.LatLng(lat, lon));
                        },

                        setZoom: function(zoom) {
                                $map.setZoom(zoom);
                        },

                        setType: function(type) {
                                switch (type) {
                                        case 'roadmap':
                                                $map.setMapTypeId(google.maps.MapTypeId.ROADMAP);
                                        break;

                                        case 'sattelite':
                                                $map.setMapTypeId(google.maps.MapTypeId.SATELLITE);
                                        break;

                                        case 'hybrid':
                                                $map.setMapTypeId(google.maps.MapTypeId.HYBRID);
                                        break;

                                        case 'terrain':
                                                $map.setMapTypeId(google.maps.MapTypeId.TERRAIN);
                                        break;
                                }
                        },

                        get: function() {
                                return $map;
                        }
                });
        },

        Track: function(map, opts) {
                var $opts = {};
                var $map = map.get();
                var $path = [];

                $.extend($opts, { color: '000000', weight: 3 }, opts);

                var $line = new google.maps.Polyline({
                        strokeColor: '#' + $opts.color,
                        strokeWeight: $opts.weight
                });

                $.extend(this, {
                        setOptions: function(options) {
                                $.extend($opts, options);
                                $line.setOptions({
                                        strokeColor: $opts.color,
                                        strokeWeight: $opts.weight
                                });
                        },

                        autoPan: false,

                        setVisible: function(visible) {
                                $line.setMap(visible ? $map : null);
                        },

                        visible: function() {
                                return $line.getMap() != null;
                        },

                        clear: function() {
                                $path.splice(0, $path.length);
                                $line.setPath($path);
                        },

                        append: function(data, size) {
                                for (var i = 0; i < size; i += 2) {
                                        $path.push(new google.maps.LatLng(data[i], data[i+1]));
                                }
                                $line.setPath($path);

                                if (this.autoPan) {
                                        this.showLast();
                                }
                        },

                        showFirst: function() {
                                if ($path.length > 1 && this.visible()) {
                                        $map.setCenter($path[0]);
                                }
                        },

                        showLast: function() {
                                if ($path.length > 1 && this.visible()) {
                                        $map.setCenter($path[$path.length - 1]);
                                }
                        }
                });
        }
}
