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

class NodeRole < ActiveRecord::Base

  attr_accessible :status
  attr_accessible :role_id, :snapshot_id, :node_id

  belongs_to      :node
  belongs_to      :role
  belongs_to      :snapshot
  has_one         :deployment,        :through => :snapshot
  has_one         :barclamp,          :through => :role

  # find other node-roles in this snapshot using their role or node
  scope           :all_by_state,      ->(state) { where(['state=?', state]) }
  scope           :committed_by_node, ->(node) { where(['state<>? AND state<>? AND node_id=?', NodeRole::PROPOSED, NodeRole::ACTIVE, node.id])}
  scope           :peers_by_state,    ->(ss,state) { where(['snapshot_id=? AND state=?', ss.id, state]) }
  scope           :peers_by_role,     ->(ss,role)  { where(['snapshot_id=? AND role_id=?', ss.id, role.id]) }
  scope           :peers_by_node,     ->(ss,node)  { where(['snapshot_id=? AND node_id=?', ss.id, node.id]) }
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
  STATES     = {
    ERROR => 'error',
    ACTIVE => 'active',
    TODO => 'todo',
    TRANSITION => 'transition',
    BLOCKED => 'blocked',
    PROPOSED => 'proposed'
  }

  class InvalidTransition < Exception
    def initialize(node_role,from,to,str=nil)
      @errstr = "#{node_role.name}: Invalid state transition from #{NodeRole.state_name(from)} to #{NodeRole.state_name(to)}"
      @errstr += ": #{str}" if str
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
    raise InvalidState.new("#{state || 'nil'} is not a valid NodeRole state!") unless state and STATES.include? state
    I18n.t(STATES[state], :scope=>'node_role.state')
  end

  def self.reset!
    NodeRole.transaction do
      NodeRole.all.each do |nr|
        nr.send(:write_attribute,"state",NodeRole::PROPOSED)
        nr.data = {}
        nr.wall = {}
        nr.save!
      end
      NodeRole.all.each do |nr|
        nr.commit!
      end
    end
  end

  # The very basic annealer.
  def self.anneal!
    queue = Hash.new
    NodeRole.transaction do
      # Check to see if we have all our jigs before we send everything off.
      NodeRole.all_by_state(NodeRole::TODO).each do |nr|
        next unless nr.role.jig_name
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
    return nil if queue.empty?
    # Actaully run the noderoles outside of the transaction.
    queue.each do |thisjig,candidates|
      candidates.each do |c|
        Rails.logger.info("Annealer: #{thisjig.name} running #{c.role.name} on #{c.node.name} for #{c.deployment.name}")
        thisjig.run(c)
        Rails.logger.info("Annealer: Run finished.")
      end
    end
    true
  end

  def self.converge!
    loop do
      break unless anneal!
    end
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

  def deployment_role
    DeploymentRole.snapshot_and_role(snapshot,role).first
  end

  def data(merge=true)
    raw = read_attribute("userdata") 
    d = raw.nil? ? {} : JSON.parse(raw)  
    merge ? deployment_role.data.deep_merge(d) : d
  end

  def data=(arg)
    raise I18n.t('node_role.cannot_edit_data') unless state==PROPOSED
    arg = JSON.generate(arg) if arg.is_a? Hash
 
    ## TODO Validate the config file
    raise I18n.t('node_role.data_parse_error') unless true

    write_attribute("userdata",arg)

  end

  def sysdata
    raw = JSON.parse(read_attribute("sysdata")||'{}')
  end

  def sysdata=(arg)
    write_attribute("userdata",JSON.generate(arg))
  end

  def data_schema
    { :type=>:map, :required=>true, :mapping=>{ } }
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

  def wall_schema
    { :type=>:map, :required=>true, :mapping=>{ } }
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

  def activatable?
    parents.all?{|p|p.active?}
  end

  # Walk returns all of the NodeRole graph (including self) reachable from
  # the current node as parents or children, depending on which method is passed
  # Found noderoles are returned in cohort order.
  def __walk(meth)
    tracked = Hash.new
    NodeRole.transaction do
      curr = [ self ]
      until curr.empty? do
        next_curr = Array.new
        curr.each do |nr|
          tracked[nr] = nr.cohort
          nr.send(meth).each do |cnr|
            next if tracked[cnr]
            tracked[cnr] = cnr.cohort
            next_curr << cnr
          end
        end
        curr = next_curr
      end
    end
    res = Array.new
    tracked.each do |k,v|
      res[v] ||= Array.new
      res[v] << k
    end
    res.compact!
    res.sort!
    res.flatten!
    res
  end

  # Return all parents and ourself in cohort order.
  def all_parents
    __walk(:parents)
  end

  # Return ourself and all our children in cohort order.
  def all_children
    __walk(:children)
  end

  # Find all the direct and indirect children, then call the block
  # on them along with the current block in cohort order.
  def walk(block)
    raise "Must be passed a block" unless block.kind_of?(Proc)
    all_children.map do |nr| block.call(nr) end
  end

  # Find all the direct and indirect parents, and then call the block
  # on them in reverse cohort order.
  def parentwalk(block)
    raise "Must be passed a block" unless block.kind_of?(Proc)
    all_parents.reverse.map do |nr| block.call(nr) end
  end

  def deployment_data
    res = {}
    DeploymentRole.where(:snapshot_id => snapshot.id,:role_id => role.id).each do |dr|
      res.deep_merge!(dr.data)
      res.deep_merge!(dr.wall)
    end
    res
  end
  
  def all_my_data
    res = {}
    res.deep_merge!(wall)
    res.deep_merge!(sysdata)
    res.deep_merge!(data)
    res
  end

  def all_deployment_data
    res = {}
    all_parents.each {|parent| res.deep_merge!(parent.deployment_data)}
    res
  end

  def all_parent_data
    res = {}
    all_parents.each do |parent| res.deep_merge!(parent.all_my_data) end
    res
  end

  def all_data
    res = all_deployment_data
    res.deep_merge!(all_parent_data)
    res
  end

  def all_transition_data
    res = all_deployment_data
    res.deep_merge!(self.node.all_active_data)
    res.deep_merge!(wall)
    res.deep_merge!(sysdata)
    res.deep_merge!(data)
    res
  end
    
  def rerun
    NodeRole.transaction do
      raise InvalidTransition(self,state,TODO,"Cannot rerun transition") unless state == ERROR
      write_attribute("state",TODO)
      save!
    end
  end
  
  # Implement the node role state transition rules
  # by guarding state assignment.
  def state=(val)
    cstate = state
    return val if val == cstate
    Rails.logger.info("NodeRole: transitioning #{self.role.name}:#{self.node.name} from #{STATES[cstate]} to #{STATES[val]}")
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
          c.walk(lambda{|n|n.state = BLOCKED})
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
          c.state = TODO if c.activatable?
        end
      when TODO
        # We can only go to TODO when:
        # 1. We were in PROPOSED or BLOCKED or ERROR
        # 2. All our parents are in ACTIVE
        unless ((cstate == PROPOSED) || (cstate == BLOCKED)) || (cstate == ERROR)
          raise InvalidTransition.new(self,cstate,val)
        end
        unless activatable?
          raise InvalidTransition.new(self,cstate,val,"Not all parents are ACTIVE")
        end
        write_attribute("state",val)
        save!
        # Going into TODO transitions all our children into BLOCKED.
        children.each do |c|
          c.walk(lambda{|n|n.state = BLOCKED})
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
          c.walk(lambda{|n|n.state = BLOCKED})
        end
      when PROPOSED
        # Only new node_roles can be in proposed
        raise InvalidTransition.new(self,cstate,val)
      else
        # No idea what this is.  Just die.
        raise InvalidState.new("Unknown state #{s.inspect}")
      end
      # Now that the state change has passed, call any hooks for the new state.
      meth = "on_#{STATES[val]}".to_sym
      role.send(meth,self)
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
  
end
