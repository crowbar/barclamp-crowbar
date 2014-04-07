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

/**
 * Utility method that acts mostly like sprintf in other languages like PHP or C.
 * The only limitation is that it does not format numbers with leading digits or
 * something similar.
 *
 * @usage "I like {0} and i eat {1}".format("orange", "steak");
 * @param value
 * @returns string
 */
if (!String.prototype.format) {
  String.prototype.format = function() {
    var args = arguments;

    return this.replace(
      /{(\d+)}/g,
      function(match, number) {
        if (typeof args[number] != 'undefined') {
          return args[number];
        } else {
          return match;
        }
      }
    );
  };
}
