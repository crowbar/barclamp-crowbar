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
 * Author: SUSE LINUX Products GmbH
 */
;(function($, doc, win) {
  'use strict';

  $.expr[':'].search = function(a,i,m) {
    return (a.textContent || a.innerText || "").toUpperCase().indexOf(m[3].toUpperCase())>=0;
  };

  function ListSearch(el, options) {
    this.$el = $(el);

    this.defaults = {
      target: null
    };

    this.options = $.extend(
      this.defaults,
      options
    );

    this.init();
  }

  ListSearch.prototype.init = function() {
    if (this.defaults.target) {
      var target = this.defaults.target;
    } else {
      var target = this.$el.data("listsearch");
    }

    this.$el.on('keyup', function(e) {
      var filter = $(this).val();

      if (filter) {
        $(target).find("li:not(:search(" + filter + "))").slideUp();
        $(target).find("li:search(" + filter + ")").slideDown();
      } else {
        $(target).find("li").slideDown();
      }

      return false;
    });
  };

  $.fn.listSearch = function(options) {
    return this.each(function() {
      new ListSearch(this, options);
    });
  };
}(jQuery, document, window));
