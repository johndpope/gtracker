/**
 * @requires OpenLayers/Layer/TMS.js
 *
 * Class: OpenLayers.Layer.Yandex
 *
 * Inherits from:
 *  - <OpenLayers.Layer.TMS>
*/
OpenLayers.Layer.Yandex = OpenLayers.Class(OpenLayers.Layer.TMS, {
   initialize: function(name, url, options) {
      options = OpenLayers.Util.extend({
         maxExtent: new OpenLayers.Bounds(-20037508,-20002151,20037508,20072865),
         type: "png",
         numZoomLevels: 18,
         transitionEffect: "resize"
      }, options);
      var newArguments = [name, url, options];
      OpenLayers.Layer.TMS.prototype.initialize.apply(this, newArguments);
   },

    /**
     * Method: getUrl
     *
     * Parameters:
     * bounds - {<OpenLayers.Bounds>}
     *
     * Returns:
     * {String} A string with the layer's url and parameters and also the
     *          passed-in bounds and appropriate tile size specified as
     *          parameters
     */
   getURL: function (bounds) {
      var res = this.map.getResolution();
      var maxExtent = (this.maxExtent) ? this.maxExtent : yandexBounds;
      var tileW = (this.tileSize)?this.tileSize.w:256;
      var tileH = (this.tileSize)?this.tileSize.h:256;
      var x = Math.round((bounds.left - maxExtent.left)/(res * tileW));
      var y = Math.round((maxExtent.top - bounds.top)/(res * tileH));
      var z = this.map.getZoom();var limit = Math.pow(2, z);
      if (y < 0 >= limit) {
         return OpenLayers.Util.getImagesLocation() + "404.png";
      } else {
         x = ((x % limit) + limit) % limit;
         url = (this.url) ? this.url : "http://vec02.maps.yandex.net/";
         return url + "tiles?l=map&v=2.2.3&x=" + x + "&y=" + y + "&z=" + z;
      }
   },

   CLASS_NAME: "OpenLayers.Layer.Yandex"
});
