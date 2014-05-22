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
 * Utility method stripping any html out of a localized string. In particular if the
 * key is not present in the localization file you'll see something like:
 * <span class="missing_transaltion">need,to,strip,html</span> which will break your
 * javascript.
 *
 * @usage $.localizedValue("<span class="missing_transaltion">need,to,strip,html</span>");
 * @param value
 * @returns string
 */
jQuery.localizedValue = function(val) {
  return $(val).text();
};
