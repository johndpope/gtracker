var GMap = {

        type: 'Yandex',

        Map: function(element) {
                var $map = new YMaps.Map(element.get(0));
                $map.enableScrollZoom();

                $.extend(this, {
                        setCenter: function(lat, lon) {
                                $map.setCenter(new YMaps.GeoPoint(lon, lat));
                        },

                        setZoom: function(zoom) {
                                $map.setZoom(zoom);
                        },

                        setType: function(type) {
                                switch (type) {
                                        case 'roadmap':
                                                $map.setType(YMaps.MapType.MAP);
                                        break;

                                        case 'sattelite':
                                                $map.setType(YMaps.MapType.SATELLITE);
                                        break;

                                        case 'hybrid':
                                                $map.setType(YMaps.MapType.HYBRID);
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
                var $style = new YMaps.Style();
                var $line = new YMaps.Polyline([], { hasBalloon: false });

                $.extend($opts, { color: '000000', weight: 3 }, opts);

                $style.lineStyle = new YMaps.LineStyle();
                $style.lineStyle.strokeColor = $opts.color || '000000';
                $style.lineStyle.strokeWidth = $opts.weight || 3;
                $line.setStyle($style);

                $.extend(this, {
                        setOptions: function(options) {
                                $.extend($opts, options);
                                $style.lineStyle.strokeColor = $opts.color;
                                $style.lineStyle.strokeWidth = $opts.weight;
                                $line.setStyle($style);
                        },

                        autoPan: false,

                        setVisible: function(visible) {
                                if (visible) {
                                        $map.addOverlay($line);
                                } else {
                                        $map.removeOverlay($line);
                                }
                        },

                        visible: function() {
                                return $line.getMap() != null;
                        },

                        clear: function() {
                                $line.splicePoints(0, $line.getNumPoints());
                        },

                        append: function(data, size) {
                                var $points = [];
                                for (var i = 0; i < size; i += 2) {
                                        $points.push(new YMaps.GeoPoint(data[i+1], data[i]));
                                }
                                $line.addPoint($points);

                                if (this.autoPan) {
                                        this.showLast();
                                }
                        },

                        showFirst: function() {
                                if ($line.getNumPoints() > 1 && this.visible()) {
                                        $map.setCenter($line.getPoint(0));
                                }
                        },

                        showLast: function() {
                                if ($line.getNumPoints() > 1 && this.visible()) {
                                        $map.setCenter($line.getPoint($line.getNumPoints() - 1));
                                }
                        }
                });
        }
}
