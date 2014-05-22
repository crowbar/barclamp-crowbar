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

if (!Array.prototype.remove) {
  Array.prototype.remove = function(from, to) {
    var rest = this.slice((to || from) + 1 || this.length);
    this.length = from < 0 ? this.length + from : from;

    return this.push.apply(this, rest);
  };
}

if (!Array.prototype.removeValue) {
  Array.prototype.removeValue = function() {
    var a = arguments;
    var L = a.length;
    var what;
    var ax;

    while (L && this.length) {
      what = a[--L];

      while ((ax = this.indexOf(what)) !== -1) {
        this.splice(ax, 1);
      }
    }

    return this;
  };
}
