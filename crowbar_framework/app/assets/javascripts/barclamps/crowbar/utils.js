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
  if ($.queryString['waiting'] != undefined) {
    setInterval(
      function() {
        var meta = $('.row[data-update]');

        $.getJSON(meta.data('update').replace('FILENAME', meta.data('file')), function(data) {
          if (data['waiting'] == false) {
            location.href = meta.data('redirect').replace('FILENAME', meta.data('file'));
          }
        });
      },
      10000
    );
  }
});
