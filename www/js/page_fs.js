var Page = {
        initMap: function(parent) {
                var $map = new GMap.Map(parent);
                $map.setCenter(55.755206, 37.623281);
                $map.setZoom(14)
                $map.setType('roadmap');

                return $map;
        },

        init: function() {
                var $map = Page.initMap($('#map'));
                var $track = new GMap.Track($map);
                $track.setVisible(true);

                $('.begin').click(function() {
                        $track.showFirst();
                });

                $('.end').click(function() {
                        $track.showLast();
                });

                GTracker.load_public_track($PATH, {
                        error: function() {
                                alert('Invalid link');
                        },

                        success: function(points, count) {
                                $track.append(points, count);
                                $track.showFirst();
                        }
                });
        }
};
