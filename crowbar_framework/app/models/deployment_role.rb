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

require 'json'

class DeploymentRole < ActiveRecord::Base

  attr_accessible :id, :role_id, :snapshot_id

  belongs_to :snapshot
  has_one    :deployment, :through => :snapshot

  belongs_to :role
  has_one    :barclamp, :through => :role

  # convenience methods
  def name
    role.name
  end

  def description
    role.description
  end

  def data
    d = read_attribute("data")
    d = role.template_data unless data && !data.empty?
    JSON.parse(d)
  end

  def data=(arg)
    write_attribute("data",JSON.generate(arg))
  end

  def wall
    JSON.parse(read_attribute("wall"))
  end

  def wall=(arg)
    write_attribute("wall",JSON.generate(arg))
  end

  
  # add a node to this deployment for this role
  def add_node(node)
    raise "you can only add node #{node.name} to a Proposed Deployment" unless snapshot.proposed?
    NodeRole.create :node_id=>node.id, :snapshot_id=>snapshot_id, :role_id=>role_id
  end

  def all_data
    JSON.parse(role.role_template).deep_merge(data)
  end
  
end
