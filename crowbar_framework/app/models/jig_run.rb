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


####
# A run represents the execution of a Jig tool on a node, to apply a role (or potentially a set of them)

class JigRun < ActiveRecord::Base
  attr_accessible :name, :description, :type, :order, :result, :status, :jig_event_id

  belongs_to      :jig_event  # the event triggering this run
  alias_attribute :event,     :jig_event
  has_one         :jig,       :through => :jig_event

  has_many        :attrib_instances
  has_many        :nodes,             :through   => :attrib_instances   # may need unique filter to eliminate dup nodes

  # The status values
  RUN_UNKNOWN = 0
  RUN_PENDING = 1
  RUN_EXECUTING = 2
  RUN_SUCCESS = 3
  RUN_FAIL = 4

  # grand-parent object (perhaps can be a belongs_to :through?)
  def jig
    self.event.jig
  end

end
