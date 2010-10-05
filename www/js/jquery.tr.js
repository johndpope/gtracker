(function($) {
        var msg = {};

        $.extend({
                tr_load: function(data) {
                        $.extend(msg, data);
                },

                tr_clear: function(data) {
                        msg = {};
                },

                tr: function(id) {
                        return msg[id] || '_undef_(' + id + ')';
                }
        });
})(jQuery);
