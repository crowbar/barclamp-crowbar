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
  after_create :role_create_hook
  before_destroy  :role_delete_hook

  belongs_to :snapshot
  has_one    :deployment, :through => :snapshot

  belongs_to :role
  has_one    :barclamp, :through => :role
  has_many   :attribs, :through => :role

  scope      :snapshot_and_role,     ->(ss,role)  { where(['snapshot_id=? AND role_id=?', ss.id, role.id]) }


  # convenience methods

  def name
    role.name
  end

  def description
    role.description
  end

  def data
    d = read_attribute("data")
    d.nil? ? {} : JSON.parse(d)
  end

  def data=(arg)
    arg = JSON.generate(arg) if arg.is_a? Hash
    # TODO validate!
    write_attribute("data",arg)
  end

  def data_update(val)
    DeploymentRole.transaction do
      d = data
      d.deep_merge!(val)
      data = d
    end
  end

  def wall
    d = read_attribute("wall")
    return {} if d.nil? || d.empty?
    JSON.parse(d)
  end

  def wall=(arg)
    arg = JSON.generate(arg) if arg.is_a? Hash
    write_attribute("wall",arg)
  end

  def wall_update(val)
    DeploymentRole.transaction do
      d = wall
      d.deep_merge!(val)
      wall = d
    end
  end

  private

  def role_create_hook
    role.on_deployment_create(self)
  end

  def role_delete_hook
    role.on_deployment_delete(self)
  end
  
end
