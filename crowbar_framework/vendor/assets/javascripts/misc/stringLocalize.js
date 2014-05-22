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

if (!String.prototype.localize) {
  String.prototype.localize = function() {
    values = {
      'barclamp.node_selector.node_duplicate': 'Node {0} is already assigned to {1}',
      'barclamp.node_selector.cluster_duplicate': 'Cluster {0} is already assigned to {1}',
      'barclamp.node_selector.outdated': 'There have been deleted old nodes removed, please save this proposal.',
      'barclamp.node_selector.no_admin': 'Failed to assign {0} to {1}, no admin nodes allowed',
      'barclamp.node_selector.no_cluster': 'Failed to assign {0} to {1}, no clusters allowed',
      'barclamp.node_selector.unique': 'Failed to assign {0} to {1}, it\'s already assigned to another role',
      'barclamp.node_selector.zero': 'Failed to assign {0} to {1}, no assignment allowed',
      'barclamp.node_selector.max_count': 'Failed to assign {0} to {1}, maximum of allowed nodes/clusters reached',
      'barclamp.node_selector.conflicting_roles': 'Node {0} cannot be assigned to both {1} and any of these roles: {2}'
    };

    if (values[this]) {
      return values[this];
    } else {
      return this;
    }
  };
}
