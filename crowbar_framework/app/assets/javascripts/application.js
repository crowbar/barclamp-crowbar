//= require jquery
//= require misc
//= require codemirror
//= require bootstrap
//= require_self
//= require branding

/**
 * Copyright 2011-2013, Dell
 * Copyright 2013, SUSE LINUX Products GmbH
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Author: Dell Crowbar Team
 * Author: SUSE LINUX Products GmbH
 */

jQuery(document).ready(function($) {
  $('textarea.editor').each(function() {
    CodeMirror.fromTextArea(this, {
      lineNumbers: true,
      matchBrackets: true,
      tabSize: 2
    });
  });

  $('[data-default]').each(function() {
    $(this).val(
      $(this).attr('data-default')
    );
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
    var $target = $($el.data('showit-target'));
    var values = $el.data('showit').split(',');

    if (!$el.data('showit-direct')) {
      $target = $target.parent();
    }

    if ($target) {
      if ($.inArray($el.val(), values) >= 0) {
        $target.show(100).removeAttr('disabled');
      } else {
        $target.hide(100).attr('disabled', 'disabled');
      }
    }
  }).trigger('change');

  $('[data-hideit]').live('change keyup', function(event) {
    var $el = $(event.target);
    var $target = $($el.data('hideit-target'));
    var values = $el.data('hideit').split(',');

    if (!$el.data('hideit-direct')) {
      $target = $target.parent();
    }

    if ($target) {
      if ($.inArray($el.val(), values) >= 0) {
        $target.hide(100).attr('disabled', 'disabled');
      } else {
        $target.show(100).removeAttr('disabled');
      }
    }
  }).trigger('change');

  $('[data-enabler]').live('change keyup', function(event) {
    var $el = $(event.target);
    var $target = $($el.data('enabler-target'));
    var values = $el.data('enabler').toString().split(',');

    if ($target) {
      if ($.inArray($el.val(), values) >= 0) {
        $target.removeAttr('disabled');
      } else {
        $target.attr('disabled', 'disabled');
      }
    }
  }).trigger('change');

  $('[data-disabler]').live('change keyup', function(event) {
    var $el = $(event.target);
    var $target = $($el.data('disabler-target'));
    var values = $el.data('disabler').toString().split(',');

    if ($target) {
      if ($.inArray($el.val(), values) >= 0) {
        $target.attr('disabled', 'disabled');
      } else {
        $target.removeAttr('disabled');
      }
    }
  }).trigger('change');

  $('[data-toggle-action]').live('click', function(e) {
    var target = '[data-toggle-target="{0}"]'.format(
      $(this).data('toggle-action')
    )

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

  $('[data-change]').updateAttribute();
  $('[data-listsearch]').listSearch();
  $('[data-ledupdate]').ledUpdate();
  $('#nodelist').nodeList();

  setInterval(
    function() {
      $('.led.failed, .led.pending, .led.waiting, led.red').toggleClass('blink');
    },
    500
  );
});
