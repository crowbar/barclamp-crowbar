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
  after_update    :cascade_state
  attr_accessible :state, :status, :data, :wall
  attr_accessible :role_id, :snapshot_id, :node_id, :turn_id

  belongs_to      :node
  belongs_to      :role
  belongs_to      :snapshot
  belongs_to      :cycle
  has_one         :deployment,        :through => :snapshot

  # find other node-roles in this snapshot using their role or node
  scope           :peers_by_role,     ->(s,r) { where(['snapshot_id=? AND role_id=?', s.id, r.id]) }
  scope           :peers_by_node,     ->(s,n) { where(['snapshot_id=? AND node_id=?', s.id, n.id]) }
  scope           :peers_by_node_and_role,     ->(s,n,r) { where(['snapshot_id=? AND role_id=? AND node_id=?', s.id, r.id, n.id]) }

  # make sure that new node-roles have require upstreams 
  # validate        :deployable,        :if => :deployable?
  has_and_belongs_to_many :parents, :class_name => "NodeRole", :join_table => "node_role_pcm", :foreign_key => "parent_id", :association_foreign_key => "child_id"
    has_and_belongs_to_many :children, :class_name => "NodeRole", :join_table => "node_role_pcm", :foreign_key => "child_id", :association_foreign_key => "parent_id"

  ERROR     = -1
  ACTIVE    = 0
  TODO      = 1
  TRANSISTION = 2
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

  def transistion?
    state > 1
  end

  def proposed?
    state.nil?
  end

  # convenience methods
  def name
    role.name
  end

  # convenience methods
  def description
    role.description
  end

  # are the upstream required node-roles included in this snapshot
  def deployable?
    role.upstream.nil? or upstream.count > 0
  end

  # unblocked? are the upstream required node-roles in a ready state
  def executable?
    upstream.all?{|p|p.ready?} and (committed? or todo?)
  end

  # return node roles (in same snapshot) who are dependent on this node-role
  def downstream
    roles = Role.downstream(role)
    roles.flatten { |r| NodeRole.peers_by_role(snapshot, r) }
  end

  # return node roles (in same snapshot) that this node-role depends on 
  def upstream
    roles = role.upstream
    roles.flatten { |r| NodeRole.peers_by_role(snapshot, r) }
  end

  private

  # on change
  # if state is set to PROPOSED, then ALL downstream states are TODO
  def cascade_state
    if snapshot.proposed?
      state = PROPOSED
      # lookup all the down stream node-roles
      role.children.each do |children|
        r.state = TODO unless d.proposed?          
      end
    end
  end

  def get_template
    data ||= role.node_template || '{}'
  end

end
