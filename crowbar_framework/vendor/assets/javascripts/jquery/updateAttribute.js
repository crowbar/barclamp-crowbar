/**
 * Copyright 2011-2013, Dell
 * Copyright 2013, SUSE LINUX Products GmbH
 *
 * Licensed under the Apache License, Version 2.0 (the 'License');
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an 'AS IS' BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Author: Dell Crowbar Team
 * Author: SUSE LINUX Products GmbH
 */
;(function($, doc, win) {
  'use strict';

  function UpdateAttribute(el, options) {
    this.$el = $(el);

    this.defaults = {
      attribute: 'change',
      storage: '#proposal_attributes'
    };

    this.options = $.extend(
      this.defaults,
      options
    );

    this.init();
  }

  UpdateAttribute.prototype.init = function() {
    var data = this.$el.data(
      this.options.attribute
    ).split(';', 3);

    var path = data[0];
    var field = data[1];
    var type = data[2];

    var $field = $('#{0}'.format(field));
    var $storage = $(this.options.storage);

    this.$el.on('change keyup', function() {
      $storage.writeJsonAttribute(
        path,
        $field.val(),
        type
      );
    });
  };

  $.fn.updateAttribute = function(options) {
    return this.each(function() {
      new UpdateAttribute(this, options);
    });
  };
}(jQuery, document, window));
