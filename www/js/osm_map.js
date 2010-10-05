var GMap = {

        type: 'OpenStreetMap',

        Map: function(element) {
                element.css('height', element.css('min-height'));
                var $map = new OpenLayers.Map({
                        numZoomLevels: 16,
                        units: 'km',
                        projection: new OpenLayers.Projection("EPSG:900913"),
                        displayProjection: new OpenLayers.Projection("EPSG:4326"),
                        controls: [new OpenLayers.Control.Navigation(), new OpenLayers.Control.KeyboardDefaults()]
                });
                var $layer = new OpenLayers.Layer.OSM();
                $map.addLayer($layer);
                $map.zoomTo(12);
                $map.render(element[0]);

                $.extend(this, {
                        setCenter: function(lat, lon) {
                                $map.setCenter(new OpenLayers.LonLat(lon, lat).transform($map.displayProjection, $map.getProjectionObject()));
                        },

                        setZoom: function(zoom) {
                                $map.zoomTo(zoom);
                        },

                        setType: function(type) {
                        },

                        get: function() {
                                return $map;
                        }
                });
        },

        Track: function(map, opts) {
                var $opts = {};
                var $map = map.get();
                var $line = new OpenLayers.Layer.Vector(new OpenLayers.Geometry.LineString([]));
                var $points = [];

                $.extend($opts, { color: '000000', weight: 3 }, opts);
                $.extend($line.style, { strokeColor: $opts.color, strokeWidth: $opts.weight });

                $.extend(this, {
                        setOptions: function(options) {
                                $.extend($opts, options);
                                $.extend($line.style, { strokeColor: $opts.color, strokeWidth: $opts.weight });
                        },

                        autoPan: false,

                        setVisible: function(visible) {
                                if (visible) {
                                        $map.addLayer($line);
                                } else {
                                        $map.removeLayer($line);
                                }
                        },

                        visible: function() {
                                return $line.map != null;
                        },

                        clear: function() {
                                $points.splice(0, $points.length);
                        },

                        append: function(data, size) {
                                for (var i = 0; i < size; i += 2) {
                                        $points.push(new OpenLayers.LonLat(data[i+1], data[i]));
                                }
                        },

                        showFirst: function() {
                                if ($points.length > 1 && this.visible()) {
                                        $map.setCenter($points[0].clone().transform($map.displayProjection, $map.getProjectionObject()));
                                }
                        },

                        showLast: function() {
                                if ($points.length > 1 && this.visible()) {
                                        $map.setCenter($points[$points.length - 1].clone().transform($map.displayProjection, $map.getProjectionObject()));
                                }
                        }
                });
        }
}
