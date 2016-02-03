//= require js-routes

//= require jquery
//= require misc
//= require codemirror
//= require bootstrap
//= require_self
//= require branding

jQuery(document).ready(function($) {
  $('textarea.editor').each(function() {
    var cm = CodeMirror.fromTextArea(this, {
      lineNumbers: true,
      matchBrackets: true,
      tabSize: 2
    });

    $(this).data('codeMirror', cm);
  });

  $('[data-default]').each(function() {
    $(this).val(
      $(this).attr('data-default')
    );
  });

  $('[data-sslprefix]').each(function() {
    var afterInit = false;
    var prefix = $(this).data('sslprefix');

    var switcher = 'select[name={0}]'.format($(this).attr('id'));
    var container = '#{0}_container'.format(prefix);
    var target = '#{0}_generate_certs'.format(prefix);

    var key = $(this).data('sslkey');
    var cert = $(this).data('sslcert');

    $(switcher).live('change', function() {
      var val = $(this).val();

      if (val == true || val == 'true' || val == 'https') {
        $(container).show(100).removeAttr('checked');
      } else {
        $(container).hide(100).attr('disabled', 'disabled');
      }
    }).trigger('change');

    $(target).live('change', function() {
      var $parent = $(
        '#{0}_certfile, #{1}_keyfile, #{2}_insecure'.format(
          prefix,
          prefix,
          prefix
        )
      );

      if ($(this).val() == 'true') {
        $parent.attr('disabled', 'disabled');

        $('#{0}_certfile'.format(prefix)).val(cert).trigger('change');
        $('#{0}_keyfile'.format(prefix)).val(key).trigger('change');
        $('#{0}_insecure option'.format(prefix)).removeAttr('selected').siblings('[value=true]').attr('selected', true).trigger('change');
      } else {
        $parent.removeAttr('disabled');

        if (afterInit) {
          $('#{0}_insecure option'.format(prefix)).removeAttr('selected').siblings('[value=false]').attr('selected', true).trigger('change');
        }
      }
    }).trigger('change');

    afterInit = true;
  });

  $('[data-piechart]').sparkline('html', {
    type: 'pie',
    tagValuesAttribute: 'data-piechart',
    disableTooltips: true,
    disableHighlight: true,
    sliceColors: [
      '#0f0',
      '#f00',
      '#999',
      '#ff0'
    ]
  });

  $('[data-blockui]').live('click submit', function(event) {
    $.blockUI({
      css: {
        border: 'none',
        padding: '15px',
        backgroundColor: '#000',
        '-webkit-border-radius': '10px',
        '-moz-border-radius': '10px',
        opacity: .5,
        color: '#fff'
      },
      message: $(event.target).data('blockui')
    });
  });

  $('[data-checkall]').live('change', function(event) {
    var checker = $(event.target).data('checkall');

    if (event.target.checked) {
      $(checker).attr('checked','checked');
    } else {
      $(checker).removeAttr('checked');
    }
  });

  $('[data-showit]').live('change keyup', function(event) {
    var $el = $(event.target);

    var targets = $el.data('showit-target').toString().split(';');
    var values = $el.data('showit').toString().split(';');

    $.each(targets, function(index, target) {
      var selects = values[index].toString().split(',');
      var $target = $(target);

      if (!$el.data('showit-direct')) {
        $target = $target.parent();
      }

      if ($target) {
        if ($.inArray($el.val(), selects) >= 0) {
          $target.show(100).removeAttr('disabled');
        } else {
          $target.hide(100).attr('disabled', 'disabled');
        }
      }
    });
  }).trigger('change');

  $('[data-hideit]').live('change keyup', function(event) {
    var $el = $(event.target);

    var targets = $el.data('hideit-target').toString().split(';');
    var values = $el.data('hideit').toString().split(';');

    $.each(targets, function(index, target) {
      var selects = values[index].toString().split(',');
      var $target = $(target);

      if (!$el.data('hideit-direct')) {
        $target = $target.parent();
      }

      if ($target) {
        if ($.inArray($el.val(), selects) >= 0) {
          $target.hide(100).attr('disabled', 'disabled');
        } else {
          $target.show(100).removeAttr('disabled');
        }
      }
    });
  }).trigger('change');

  $('[data-enabler]').live('change keyup', function(event) {
    var $el = $(event.target);

    var targets = $el.data('enabler-target').toString().split(';');
    var values = $el.data('enabler').toString().split(';');

    $.each(targets, function(index, target) {
      var selects = values[index].toString().split(',');
      var $target = $(target);

      if ($target) {
        if ($.inArray($el.val(), selects) >= 0) {
          $target.removeAttr('disabled');
        } else {
          $target.attr('disabled', 'disabled');
        }
      }
    });
  }).trigger('change');

  $('[data-disabler]').live('change keyup', function(event) {
    var $el = $(event.target);

    var targets = $el.data('disabler-target').toString().split(';');
    var values = $el.data('disabler').toString().split(';');

    $.each(targets, function(index, target) {
      var selects = values[index].toString().split(',');
      var $target = $(target);

      if ($target) {
        if ($.inArray($el.val(), selects) >= 0) {
          $target.attr('disabled', 'disabled');
        } else {
          $target.removeAttr('disabled');
        }
      }
    });
  }).trigger('change');

  $('[data-toggle-action]').live('click', function(e) {
    var target = '[data-toggle-target="{0}"]'.format(
      $(this).data('toggle-action')
    );

    e.preventDefault();

    if ($(target).hasClass('hidden')) {
      $(this).find('span').switchClass(
        "glyphicon-chevron-right",
        "glyphicon-chevron-down",
        0
      );

      $(target).switchClass(
        "hidden",
        "visible",
        0
      );
    } else {
      $(this).find('span').switchClass(
        "glyphicon-chevron-down",
        "glyphicon-chevron-right",
        0
      );

      $(target).switchClass(
        "visible",
        "hidden",
        0
      );
    }
  });

  $('[data-tooltip]').tooltip({
    html: true
  });

  $('[data-dynamic]').dynamicTable();
  $('[data-change]').updateAttribute();
  $('[data-listsearch]').listSearch();
  $('[data-ledupdate]').ledUpdate();
  $('[data-show-for-clusters-only="true"]').hideShowClusterConf();

  $('#proposal_attributes, #proposal_deployment').changedState();
  $('#nodelist').nodeList();
  $('input[type=password]').hideShowPassword();
  $('[data-hidetext]').hideShowText();

  setInterval(
    function() {
      $('.led.failed, .led.pending, .led.waiting, led.red').toggleClass('blink');
    },
    500
  );
});
