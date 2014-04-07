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

  $.queryString = (function(a) {
    if (a == "") {
      return {};
    }

    var b = {};

    for (var i = 0; i < a.length; ++i) {
      var p = a[i].split('=');

      if (p.length != 2) {
        continue;
      }

      b[p[0]] = decodeURIComponent(
        p[1].replace(/\+/g, " ")
      );
    }

    return b;
  })(window.location.search.substr(1).split('&'));
}(jQuery, document, window));
