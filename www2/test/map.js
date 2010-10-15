var Map = {
   Map:     function(element_id) {
               var $options = {
                  projection: new OpenLayers.Projection("EPSG:900913"),
                  displayProjection: new OpenLayers.Projection("EPSG:4326"),
                  units: "m",
                  maxResolution: 156543.0339,
                  maxExtent: new OpenLayers.Bounds(-20037508.34, -20037508.34,
                        20037508.34, 20037508.34)
               };

               var $map = new OpenLayers.Map(element_id, $options); 
               $map.addLayers([
                     new OpenLayers.Layer.OSM("OpenStreetMap"),
                     new OpenLayers.Layer.Google("Google", { sphericalMercator: true })
                     ]);

               $map.addControl(new OpenLayers.Control.LayerSwitcher());

               $.extend(this, {
                  setCenter:   function($lon, $lat) {
                                  $map.setCenter(new OpenLayers.LonLat($lon, $lat).
                                     transform($map.displayProjection, $map.projection));
                               },

                  setZoom:     function($zoom) {
                                  $map.zoomTo($zoom);
                               },

                  addTrack:    function($track) {
                                  $map.addLayer($track);
                               },

                  removeTrack: function($track) {
                                  $map.removeLayer($track);
                               }
               });
            },

   Track:   function($name, $options) {
               var $layer_style = OpenLayers.Util.extend({}, OpenLayers.Feature.Vector.style['default']);
               $layer_style.fillOpacity = 0.2;
               $layer_style.graphicOpacity = 1;

               var $opts = {};
               $.extend($opts, { color: '#FF0000', weight: 3 }, $options);

               var $line_style = {
                  strokeColor: $opts.color,
                  strokeWidth: $opts.weight,
                  pointRadius: 6,
                  pointerEvents: "visiblePainted"
               };

               var $geometry = new OpenLayers.Geometry.LineString([]);

               var $line = new OpenLayers.Feature.Vector($geometry, null, $line_style);

               var $vector = new OpenLayers.Layer.Vector($name, {style: $layer_style});
               $vector.addFeatures([$line]);

               $.extend(this, {
                  append:  function(data) {
                           },
                  clear:   function() {
                           }
               });
            }
}
