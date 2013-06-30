# Copyright 2013, Dell
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

class NodeRole < ActiveRecord::Base

  before_create   :get_template
  attr_accessible :state, :status, :data, :wall
  attr_accessible :role_id, :snapshot_id, :node_id

  belongs_to      :node
  belongs_to      :role
  belongs_to      :snapshot
  has_one         :deployment, :through => :snapshot

  ERROR     = -1
  ACTIVE    = 0
  TODO      = 1
  COMMITTED = 2
  PROPOSED  = nil

  def error?
    state < 0
  end

  def active?
    state == 0
  end

  def todo?
    state == 1
  end

  def committed?
    state > 1
  end

  def proposed?
    state.nil?
  end

  # convenience methods
  def name
    role.name
  end

  def description
    role.description
  end
  
  private

  def get_template
    data ||= role.node_template
  end

end