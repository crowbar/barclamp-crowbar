(function(a){a.fn.konami=function(b,c){c=a.extend({},a.fn.konami.params,c);this.each(function(){var d=a(this);d.bind("konami",b).bind("keyup",function(e){a.fn.konami.checkCode(e,c,d);});});return this;};a.fn.konami.params={code:[38,38,40,40,37,39,37,39,66,65],step:0};a.fn.konami.checkCode=function(b,c,d){if(b.keyCode==c.code[c.step]){c.step++;}else{c.step=0;}if(c.step==c.code.length){d.trigger("konami");c.step=0;}};})(jQuery);
var piechart_options = {type: 'pie', width: '17px', height: '17px', sliceColors: ["#0f0", "#f00", "#999", "#ff0"] };

/**
 * Utility method stripping any html out of a localized string.  In particular if the 
 * key is not present in the localization file you'll see something like:
 * <span class="missing_transaltion">need,to,strip,html</span> which will break your javascript.
 * 
 * @usage $.localizedValue("<span class="missing_transaltion">need,to,strip,html</span>");
 * @param value
 * @returns string
 */
jQuery.localizedValue = function(val){ 
	return $(val).text();	
};

jQuery(document).ready(function($) {
  
  $('textarea.editor').each(function(index) {
    CodeMirror.fromTextArea(this, {
      lineNumbers: true,
      matchBrackets: true
    });
  });
  
  $('a.node_details').click(function(e) {
    selected = $(this).attr('id');
    $('#details').load($(this).attr('href'));
    $('tr.selected').removeClass('selected');
    $(this).parents('tr').addClass('selected');
    e.preventDefault();
  });
  
  $('#details .roles a').live('click', function(e) {
    link = $(this);
    $.getJSON(link.attr('href'), function(data) {
      $('a.highlight').removeClass('highlight');
      link.addClass('highlight');
      $('tr.selected').removeClass('selected');
      $.each(data['nodes'], function(i,node) {
        $('tr#'+node).addClass('selected');
      });
    });
    e.preventDefault();
  });
  
  $('#details .barclamps a').live('click', function(e) {
    link = $(this);
    $.getJSON(link.attr('href'), function(data) {
      $('a.highlight').removeClass('highlight');
      link.addClass('highlight');
      $('tr.selected').removeClass('selected');
      $.each(data['nodes'], function(i,node) {
        $('tr#'+node).addClass('selected');
      });
    });
    e.preventDefault();
  });
  
  $('.inline_piechart').sparkline('html', piechart_options );
  
  // Blinking lights
  setInterval( function() {
    $('.led.failed, .led.pending, .led.waiting, led.red').toggleClass('blink');
  }, 500);

  // Animate spinning LEDs
  function animate() {
    $('.led.unready, .led.in_process, .led.spin').sprite({fps: 6, no_of_frames: 8});
  }

  // Stop Animate spinning LEDs
  function deanimate() {
    $('.led.unready, .led.in_process, .led.spin').destroy();
  }

  animate(); // Call this again when new animatable elements are created...
  
  // Auto-run update functions periodically
  if(typeof update == 'function') { 
    setInterval(function() {
      update();
      deanimate();
      animate();
    }, 10000);
  }

  $('.button').live('click', function() {
    var button = $(this);
    button2 = $('.button[source="'+button.attr('match')+'"]');
    $('#flash').attr("style", "display:none");
    button.addClass('pressed');
    if (button2) button2.addClass('pressed');
    if(button.attr('data-remote')=='true') {
      button.bind('ajax:complete', function(){ button.removeClass('pressed'); });
      if (button2) button2.bind('ajax:complete', function(){ button2.removeClass('pressed'); });
    }
  });
  
  $('input[data-default]').each(function() {
    $(this).val($(this).attr('data-default'))
  })
  $('input[data-default-clear]').each(function() {
    $(this).val($(this).attr('data-default-clear')).addClass('default');
  }).click(function(e){
    $(this).val('').removeClass('default');
  });
  // Toggle stuff
  $('.toggle').not('.disabled').click(function() {
    targets = $(this).attr('rel').split(',');
    $(this).toggleClass('on');
    $.each(targets, function(index, target) {
      try { target = target.trim(); } catch(err) { true; }
      if( $(this).attr('data-speed') ) {
        $('#'+target).slideToggle( $(this).attr('data-speed') );
      } else {
        $('#'+target).toggle();
      }
    });
  });
  
  $(document).konami(function(){
    $("header h1 a").css('background-image','url("/images/layout/bunny.png")').css('width','279px');
  });
});
