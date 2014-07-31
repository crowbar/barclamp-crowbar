/**
 * Copyright 2011-2013, Dell
 * Copyright 2013-2014, SUSE LINUX Products GmbH
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
 */
;(function($, doc, win) {
  'use strict';

  function ChangedState(el, options) {
    this.$el = $(el);

    this.defaults = {
      attribute: 'changedState',
      sourceEvents: 'change',
      targetElements: 'a.customview, a.rawview, .nav a:not([data-toggle]), .btn.cancel',
      targetEvents: 'click',
      message: this.$el.data('changed-state')
    };

    this.options = $.extend(
      this.defaults,
      options
    );

    this.init();
  }

  ChangedState.prototype.init = function() {
    var self = this;
    var state = false;

    var cm = this.$el.data('codeMirror');

    this.$el.live(
      self.options.sourceEvents,
      function() {
        state = true;
      }
    );

    if (cm) {
      cm.setOption('onChange', function() {
        state = true;
      });
    }

    $(self.options.targetElements).live(
      self.options.targetEvents,
      function(event) {
        if (state) {
          if (!confirm(self.options.message)) {
            event.preventDefault();
          }
        }
      }
    );
  };

  $.fn.changedState = function(options) {
    return this.each(function() {
      new ChangedState(this, options);
    });
  };
}(jQuery, document, window));
