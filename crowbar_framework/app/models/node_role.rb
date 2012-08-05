# Copyright 2012, Dell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

#############
# Node_role is an association class between a role in a proposal configuration and the
# node that is assigned that role. This supports a many2many association between
# roles and nodes, with some extra info.

class NodeRole < ActiveRecord::Base
  attr_accessible :role_name, :config, :status
  has_many        :node
  belongs_to      :role

  def deep_clone
    new_nr = self.dup
    new_nr.save

    # GREG: Clone the node_role config table one day.  It still needs work.

    new_nr
  end

end
