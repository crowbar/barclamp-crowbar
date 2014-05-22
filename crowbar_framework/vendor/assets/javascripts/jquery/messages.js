;(function($, doc, win) {
  'use strict';

  $.createMessage = function(type, msg, dismiss, disappear) {
    var assignedClasses = [
      'alert-{0}'.format(type)
    ];

    var messagteTemplate = Handlebars.compile(
      $('#message-template').html()
    );

    if (dismiss) {
      assignedClasses.push('alert-dismissable');
    }

    var renderedMessage = $(messagteTemplate({
      classes: assignedClasses.join(' '),
      message: msg,
      dismiss: dismiss
    })).uniqueId();

    if (disappear) {
      win.setTimeout(
        function() {
          renderedMessage.slideUp(500, function() {
            $(this).remove();
          });
        },
        3000
      );
    }

    return renderedMessage;
  };

  $.successMessage = function(msg, dismiss, disappear) {
    return $.createMessage('success', msg, dismiss, disappear);
  };

  $.infoMessage = function(msg, dismiss, disappear) {
    return $.createMessage('info', msg, dismiss, disappear);
  };

  $.warningMessage = function(msg, dismiss, disappear) {
    return $.createMessage('warning', msg, dismiss, disappear);
  };

  $.dangerMessage = function(msg, dismiss, disappear) {
    return $.createMessage('danger', msg, dismiss, disappear);
  };
})(jQuery, document, window);
