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


class Role < ActiveRecord::Base

  class Role::MISSING_DEP < Exception
  end
  attr_accessible :id, :description, :name, :jig_name, :barclamp_id
  attr_accessible :library, :implicit, :bootstrap, :discovery     # flags
  attr_accessible :role_template, :node_template, :min_nodes      # template info

  validates_uniqueness_of   :name,  :scope => :barclamp_id
  validates_format_of       :name,  :with=>/^[a-zA-Z][-_a-zA-Z0-9]*$/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")

  belongs_to      :barclamp

  has_many        :attribs,           :dependent => :destroy

  has_many        :role_requires,     :dependent => :destroy
  alias_attribute :requires,          :role_requires

  #has_many        :upstreams,         :through => :role_requires
  #scope           :downstreams,       ->(r) { joins(:role_requires).where(['requires=?', r.name]) }
  scope           :library,            -> { where(:library=>true) }
  scope           :implicit,           -> { where(:implicit=>true) }
  scope           :discovery,          -> { where(:discovery=>true) }
  scope           :bootstrap,          -> { where(:bootstrap=>true) }

  def parents
    role_requires.map do |r|
      Role.find_by_name!(r.requires)
    end
  end

  def depends_on?(other)
    return false if self.id == other.id
    p = parents
    return false if p.empty?
    return true if p.any?{|i|i.id == other.id}
    p.each do |i|
      return true if i.depends_on?(other)
    end
    false
  end

  # Bind a role to a node in a snapshot.
  def add_to_node_in_snapshot(node,snap)
    NodeRole.transaction do
      # If we are already bound to this node in a snapshot, do nothing.
      res = NodeRole.peers_by_node_and_role(snap,node,self).first
      return res if res
      # Check to make sure that all my parent roles are bound properly.
      # If they are not, die unless it is an implicit role.
      # This logic will need to change as we start allowing roles to classify
      # nodes, but it will work for now.
      parent_node_roles = Array.new
      parents.each do |parent|
        # This will need to grow more ornate once we start allowing multiple
        # deployments.
        pnr = NodeRole.peers_by_role(snap,parent).first
        if pnr.nil?
          if parent.implicit
            pnr = parent.add_to_node_in_snapshot(node,snap)
          else
            raise MISSING_DEP.new("Role #{name} depends on role #{parent.name}, but #{parent.name} does not exist in deployment #{snap.deployment.name}")
          end
        end
        parent_node_roles << pnr
      end
      # By the time we get here, all our parents are bound recursively.
      # Bind ourselves the same way.
      res = NodeRole.create({:node => node, :role => self, :snapshot => snap}, :without_protection => true)
      parent_node_roles.each do |pnr|
        res.parents << pnr
      end
      return res
    end
  end

  def jig
    Jig.where(["name = ?",jig_name]).first
  end

  def <=>(other)
    return 0  if self.id == other.id
    return 1  if self.depends_on?(other)
    return -1 if other.depends_on?(self)
    0
  end

  # allows role to have crowbar internal actions based on being executed
  # if there is a problem, 
  #   1) set the node-role state to error & status to a description of the error,
  #   2) raise an error!
  def on_commit(node_role)
    case node_role.role.name
      when "network-admin"
        # allocate IP
      else
        # do nothing
    end
  end

  # POSSIBLE OTHER EVENTS
  # def on_change(node)         -> returns nil or raise
  # def on_pre_execute(node_role)  -> returns nil or raise
  # def on_post_execute(node_role) -> returns nil or raise
  # def on_proposed(deployment) -> returns nodes w/ weights, # of required & desired nodes


end
