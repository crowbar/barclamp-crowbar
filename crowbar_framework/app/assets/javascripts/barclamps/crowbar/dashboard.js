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
  $('.list-group-item a').live('click', function(event) {
    $('.list-group-item').removeClass('selected');
    $(this).parents('.list-group-item').addClass('selected');

    $('#nodedetails').load(
      '{0} .panel'.format($(this).data('href'))
    );
  });

  $('[data-group-add]').live('submit', function(event) {
    var $el = $(event.target);
    var $input = $el.find('input[name=group]');

    $('.group-invalid').remove();

    var groupTemplate = Handlebars.compile(
      $('#group-panel').html()
    );

    var invalidTemplate = Handlebars.compile(
      $('#group-invalid').html()
    );

    if ($input.val().match(/^[a-zA-Z][a-zA-Z0-9._:-]+$/)) {
      $('#nodegroups').append(
        groupTemplate({
          group: $input.val()
        })
      );

      $input.val('');

      $('[data-droppable=true]:last').droppable({
        hoverClass: 'targeted',
        drop: function(event, ui) {
          var $group = $(event.target);
          var $node = $(ui.draggable.context);

          var inserted = $.map(
            $group.find('li[data-draggable=true]'),
            function(node) {
              return $(node).data('id');
            }
          );

          if ($.inArray($node.data('id'), inserted) < 0) {
            $.post(
              decodeURI(
                $node.data('update')
              ).format(
                  $group.data('id')
                ),
              function() {
                if ($group.data('id') == 'AUTOMATIC') {
                  location.reload();
                  return true;
                }

                var $ul = $group.find('ul');
                var $oldUl = $node.parents('ul');

                $ul.append(
                  $node
                );

                $ul.find('li.empty').remove();

                /*
                After reordering list items we loose the draggable functionality

                $ul.html(
                  $ul.children().sort(function(a, b) {
                    return $(a).text().toUpperCase().localeCompare($(b).text().toUpperCase());
                  })
                );
                */

                if ($oldUl.children().length <= 0) {
                  $oldUl.parents('[data-droppable=true]').remove();
                }
              }
            );
          }

          return false;
        }
      });
    } else {
      $('#content').prepend(
        invalidTemplate()
      );
    }

    event.preventDefault();
  });

  $('[data-group-delete]').live('click', function(event) {
    var $el = $(event.target);
    var $panel = $el.parents('.group-panel');

    var nodes = $panel.find('ul').children(':not(.empty)');

    if (nodes.length <= 0) {
      $panel.hide();
    }

    event.preventDefault();
  });

  $("[data-draggable=true]").draggable({
    opacity: 0.9,
    helper: "clone"
  });

  $("[data-droppable=true]").droppable({
    hoverClass: 'targeted',
    drop: function(event, ui) {
      var $group = $(event.target);
      var $node = $(ui.draggable.context);

      var inserted = $.map(
        $group.find('li[data-draggable=true]'),
        function(node) {
          return $(node).data('id');
        }
      );

      if ($.inArray($node.data('id'), inserted) < 0) {
        $.post(
          $node.data('update'),
          {
            group: $group.data('id')
          },
          function() {
            if ($group.data('id') == 'AUTOMATIC') {
              location.reload();
              return true;
            }

            var $ul = $group.find('ul');
            var $oldUl = $node.parents('ul');

            $ul.append(
              $node
            );

            $ul.find('li.empty').remove();

            /*
            After reordering list items we loose the draggable functionality

            $ul.html(
              $ul.children().sort(function(a, b) {
                return $(a).text().toUpperCase().localeCompare($(b).text().toUpperCase());
              })
            );
            */

            if ($oldUl.children().length <= 0) {
              $oldUl.parents('[data-droppable=true]').remove();
            }
          }
        );
      }

      return false;
    }
  });
});
