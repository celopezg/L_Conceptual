var widgets=$('.html-widget')

widgets.each(function( index ) {
	var button =$("<button type=\"button\" class=\"btn btn-primary btn-block full-screen-button\">Pantalla Completa <i class=\"fa fa-lg fa-expand\"></i></button>")
  	button.data('widget',$(this))
  	$( this ).after( button );
});

$( ".full-screen-button" ).click(function() {
	var widget = $( this ).data('widget')
	widget.first().fullScreenHelper('request')
});

$(document).on("fullscreenchange", function () {
  if (!$.fullScreenHelper("state")) {
   	window.dispatchEvent(new Event('resize'));
  } 
});




