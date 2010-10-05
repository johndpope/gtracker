(function($) {
        var default_hint = 'Enter';

        $.fn.blurNotify = function(hint) {
                hint = hint || default_hint;
                return this.each(function() {
                        $(this).blur(function() {
                                if ($(this).val() == '') {
                                        $(this).val(hint); 
                                }
                        });
                        $(this).focus(function() {
                                if ($(this).val() == hint) {
                                        $(this).val(''); 
                                }
                        });
                        $(this).blur();
                });
        };
})(jQuery);
