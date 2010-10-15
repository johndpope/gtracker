var lat=47.496792
var lon=7.571726
var zoom=13

var map; //complex object of type OpenLayers.Map

function init() {
   map = new OpenLayers.Map ("map", {
      controls:[
         new OpenLayers.Control.Navigation(),
         new OpenLayers.Control.PanZoomBar(),
         new OpenLayers.Control.LayerSwitcher(),
         new OpenLayers.Control.Attribution()],
         maxExtent: new OpenLayers.Bounds(-20037508.34,-20037508.34,20037508.34,20037508.34),
         maxResolution: 156543.0399,
         numZoomLevels: 19,
         units: 'm',
         projection: new OpenLayers.Projection("EPSG:900913"),
         displayProjection: new OpenLayers.Projection("EPSG:4326")
   } );

   // Define the map layer
   // Here we use a predefined layer that will be kept up to date with URL changes
   layerMapnik = new OpenLayers.Layer.OSM.Mapnik("Mapnik");
   map.addLayer(layerMapnik);
   layerTilesAtHome = new OpenLayers.Layer.OSM.Osmarender("Osmarender");
   map.addLayer(layerTilesAtHome);
   layerCycleMap = new OpenLayers.Layer.OSM.CycleMap("CycleMap");
   map.addLayer(layerCycleMap);
   layerMarkers = new OpenLayers.Layer.Markers("Markers");
   map.addLayer(layerMarkers);
}
