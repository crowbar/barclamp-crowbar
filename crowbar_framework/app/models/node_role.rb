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
  attr_accessible :status, :data, :wall
  attr_accessible :role_id, :snapshot_id, :node_id, :turn_id

  belongs_to      :node
  belongs_to      :role
  belongs_to      :snapshot
  belongs_to      :cycle
  has_one         :deployment,        :through => :snapshot
  has_one         :barclamp,          :through => :role

  # find other node-roles in this snapshot using their role or node
  scope           :peers_by_role,     ->(s,r) { where(['snapshot_id=? AND role_id=?', s.id, r.id]) }
  scope           :peers_by_node,     ->(s,n) { where(['snapshot_id=? AND node_id=?', s.id, n.id]) }
  scope           :peers_by_node_and_role,     ->(s,n,r) { where(['snapshot_id=? AND role_id=? AND node_id=?', s.id, r.id, n.id]) }

  # make sure that new node-roles have require upstreams 
  # validate        :deployable,        :if => :deployable?
  has_and_belongs_to_many :parents, :class_name => "NodeRole", :join_table => "node_role_pcms", :foreign_key => "parent_id", :association_foreign_key => "child_id"
    has_and_belongs_to_many :children, :class_name => "NodeRole", :join_table => "node_role_pcms", :foreign_key => "child_id", :association_foreign_key => "parent_id"

  # State transitions:
  # All node roles start life in the PROPOSED state.
  # At snapshot commit time, all node roles in PROPOSED that:
  #  1. Have no parent node role, or 
  #  2. Have a parent in ACTIVE state
  # will be placed in TODO state, and all others will be placed in BLOCKED.
  #
  # The annealer will then find all node roles in the TODO state, set them
  # to TRANSITION, and hand them over to their appropriate jigs.
  #
  # If the operation for the node role succeeds, the jig will set the
  # node_role to ACTIVE, set all the node_role's BLOCKED children to TODO, and
  # wake up the annealer for another pass.
  #
  # If the operation for the node role fails, the jig will set the node_role to
  # ERROR, set all of its children (recursively) to BLOCKED, and no further
  # processing for that node role dependency tree will happen.

  ERROR      = -1
  ACTIVE     =  0
  TODO       =  1
  TRANSITION =  2
  BLOCKED    =  3
  PROPOSED   =  4

  class InvalidTransition < Exception
    def initialize(node_role,from,to,str=nil)
      @errstr = "#{node_role.name}: Invalid state transition from #{NodeRole.state_name(from)} to #{NodeRole.state_name(to)}"
      errstr += ": #{str}" if str
    end
    def to_s
      @errstr
    end
    def to_str
      to_s
    end
  end

  class InvalidState < Exception
  end

  class MissingJig < Exception
    def initalize(nr)
      @errstr = "NodeRole #{nr.name}: Missing jig #{nr.jig_name}"
    end
    def to_s
      @errstr
    end
    def to_str
      to_s
    end
  end

  # lookup i18n version of state
  def state_name
    NodeRole.state_name(state)
  end

  def self.state_name(state)
    raise InvalidState.new("#{state || 'nil'} is not a valid NodeRole state!") unless state and state.between?(ERROR, PROPOSED)
    I18n.t(state.to_s, :default=>'Unknown', :scope=>'node_role.state')
  end

  def self.anneal!
    # A very basic annealer.
    queue = Hash.new
    NodeRole.transaction do
      # Check to see if we have all our jigs before we send everything off.
      NodeRole.where(["state = ?",NodeRole::TODO]).each do |nr|
        thisjig = nr.jig
        raise MissingJig.new(nr) unless thisjig.kind_of?(Jig)
        queue[thisjig] ||= []
        queue[thisjig] << nr
      end
      # Only set the candidate states inside the transaction.
      queue.each do |thisjig,candidates|
        candidates.each do |c|
          c.state = NodeRole::TRANSITION
        end
      end
    end
    # Actaully run the noderoles outside of the transaction.
    queue.each do |thisjig,candidates|
      candidates.each do |c|
        thisjig.run(c)
      end
    end
    nil
  end

  def state
    read_attribute("state")
  end

  def error?
    state == ERROR
  end

  # convenience methods
  def name
    "#{deployment.name}: #{node.name}: #{role.name}" rescue I18n.t('unknown')
  end

  def active?
    state == ACTIVE
  end

  def todo?
    state == TODO
  end

  def transistion?
    state == TRANSITION
  end

  def blocked?
    state == BLOCKED
  end

  def proposed?
    state == PROPOSED
  end

  def walk(&block)
    raise "Must be passed a block" unless block_given?
    block.call(self)
    children.each do |c|
      c.walk(block)
    end
  end

  # Implement the node role state transition rules
  # by guarding state assignment.
  def state=(val)
    cstate = state
    return val if val == cstate

    NodeRole.transaction do
      case val
      when ERROR
        # We can only go to ERROR from TRANSITION
        unless cstate == TRANSITION
          raise InvalidTransition.new(self,cstate,val)
        end
        write_attribute("state",val)
        save!
        # All children of a node_role in ERROR go to BLOCKED.
        children.each do |c|
          c.walk{|n|n.state = BLOCKED}
        end
      when ACTIVE
        # We can only go to ACTIVE from TRANSITION
        unless cstate == TRANSITION
          raise InvalidTransition.new(self,cstate,val)
        end
        write_attribute("state",val)
        save!
        # Immediate children of an ACTIVE node go to TODO
        children.each do |c|
          c.state = TODO
        end
      when TODO
        # We can only go to TODO when:
        # 1. We were in PROPOSED or BLOCKED
        # 2. All our parents are in ACTIVE
        unless ((cstate == PROPOSED) || (cstate == BLOCKED))
          raise InvalidTransition.new(self,cstate,val)
        end
        unless parents.all?{|nr|nr.active?}
          raise InvalidTransition.new(self,cstate,val,"Not all parents are ACTIVE")
        end
        write_attribute("state",val)
        save!
        # Going into TODO transitions all our children into BLOCKED.
        children.each do |c|
          c.walk{|n|n.state = BLOCKED}
        end
      when TRANSITION
        # We can only go to TRANSITION from TODO
        # As an optimization, we may also want to allow a transition from
        # BLOCKED to TRANSITION directly -- the goal would be to allow a jig
        # to batch up noderole runs by noticing that a noderole it was handed
        # in TRANSITION has children on the same node utilizing the same jig
        # in BLOCKED, and preemptivly grabbing them to batch them up.
        unless cstate == TODO
          raise InvalidTransition.new(self,cstate,val)
        end
        write_attribute("state",val)
        save!
      when BLOCKED
        # We can only go to BLOCKED from PROPOSED or TODO,
        # or if any our parents are in BLOCKED or TODO or ERROR.
        unless parents.any?{|nr|nr.blocked? || nr.todo? || nr.error?} ||
            (cstate == PROPOSED || cstate == TODO)
          raise InvalidTransition.new(self,cstate,val)
        end
        write_attribute("state",val)
        save!
        # If we are blocked, so are all our children.
        children.each do |c|
          c.walk{|n|n.state = BLOCKED}
        end
      when PROPOSED
        # Only new node_roles can be in proposed
        raise InvalidTransition.new(self,cstate,val)
      else
        # No idea what this is.  Just die.
        raise InvalidState.new("Unknown state #{s.inspect}")
      end
    end
    self
  end
  
  # convenience methods
  def name
    "#{deployment.name}: #{node.name}: #{role.name}" rescue I18n.t('unknown')
  end

  def commit!
    cstate = state
    # commit! is a no-op for ACTIVE, TRANSITION, or TODO
    return if (cstate == ACTIVE) || (cstate == TRANSITION) || (cstate == TODO)
    # You cannot commit your way out of an error state.
    raise InvalidTransition(self,cstate,TODO) if cstate == ERROR
    NodeRole.transaction do
      rents = parents
      if rents.empty? || rents.all?{|nr|nr.active?}
        self.state = TODO
      else
        self.state = BLOCKED
      end
    end
  end

  # convenience methods
  def description
    role.description
  end

  def jig
    role.jig
  end

  def get_template
    data ||= (role.node_template || '{}') rescue data = '{}'
  end

end
