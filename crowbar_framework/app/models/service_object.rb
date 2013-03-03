# Copyright 2011, Dell 
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

require 'chef'
require 'json'

#
# ServiceObject is the base object for all Barclamp service objects.
# It provides the basic barclamp operations that can be overriden by
# the barclamp object.
#
# This acts a place helper routines as well.  Locking routines, random_password, ...
#
# The object operations by initialization from the controller or the barclamp object.
# When initialize from those places, the object gets a @barclamp object that can be used
# to reference its parent barclamp.  
#
# THIS OBJECT SHOULD NEVER BE DIRECTLY USED!!
#
# operations.function = Inside the barclamp controller for this object type
# Barclamp.find_by_name("name").operations(@logger).function = Outside if the barclamp controller
#
# 
# 

class ServiceObject
  
  def self.bc_name
    Rails.logger.warn "Service object depricated"
    self.name.underscore[/(.*)_service$/,1]
  end

  #
  # Initialization setup routines
  #
  def initialize(thelogger)
    Rails.logger.warn "Service object depricated"
    @bc_name = "unknown"
    @logger = thelogger || Rails.logger
  end

  def bc_name=(new_name)
    Rails.logger.warn "Service object depricated"
    @bc_name = new_name
    @barclamp = Barclamp.find_by_name(@bc_name)
  end
  
  def bc_name 
    Rails.logger.warn "Service object depricated"
    Barclamp.find_by_name('crowbar').name
  end
  
  def logger
    Rails.logger.warn "Service object depricated"
    @logger
  end

  def barclamp
    Rails.logger.warn "Service object depricated"
    Barclamp.find_by_name 'crowbar'
  end

  #
  # Human printable random password generator
  # CB1
  def random_password(size = 12)
    # Depricated & Moved
    BarclampCrowbar::Barclamp.random_password size
  end

#
# API Functions
#
  def transition(prop_name, node_name, state)
    barclamp.transistion prop_name, node_name, state
  end

  #
  # Function to handle the barclamp controller API request to delete active proposal (deactivate)
  # Input:
  #   prop_name - String name of a proposal to deactivate
  #
  # Output:
  #   [ HTTP Error Code, String Message ]
  # CB1
  def destroy_active(prop_name)
    barclamp.deployments.first.commit_deallocate_applied
  end

  #
  # Function to handle the barclamp controller API request to dequeue proposal (dequeue)
  # Input:
  #   inst - String name of a proposal to dequeue
  #
  # Output:
  #   [ HTTP Error Code, String Message ]
  # CB1
  def dequeue_proposal(inst)
    barclamp.deployments.first.recall
  end

  #
  # Helper routine: Updates current_config
  # CB1
  def _proposal_update(new_prop, params)
    # replaced with Attributes 
  end

  #
  # This can be overridden to provide a better creation proposal
  #
  # The @barclamp.create_proposal routine must be called to create the base objects.
  # CB1
  def create_proposal(name)
    barclamp.create_deployment name
  end

  #
  # This can be overridden to provide a better creation proposal
  #
  # The @barclamp.commit_proposal routine must be called to create the base objects.
  # CB1
  def commit_proposal(proposal)
    barclamp.deployments.first.commit
  end


  #
  # Function to handle the barclamp controller API request to create a proposal
  # Input:
  #   params - params object from the controller (JSON config blob as a hash)
  #
  # Output:
  #   [ HTTP Error Code, String Message ]
  # CB1
  def proposal_create(params)
    barclamp.deployment_create params
  end

  #
  # Function to handle the barclamp controller API request to edit a proposal
  #
  # Input:
  #   params - params object from the controller (JSON config blob as a hash)
  #
  # Output:
  #   [ HTTP Error Code, String Message ]
  # CB1
  def proposal_edit(params)
    # TODO, we could make this work, but....
    # basically, we'd iterate the params and attack to attibs
  end

  #
  # Function to handle the barclamp controller API request to delete proposal (proposal delete)
  # Input:
  #   inst - String name of a proposal to delete
  #
  # Output:
  #   [ HTTP Error Code, String Message ]
  # CB1
  def proposal_delete(inst)
    barclamp.deployments.first.delete 
  end

  #
  # Function to handle the barclamp controller API request to commit proposal (proposal commit)
  # Input:
  #   inst - String name of a proposal to commit
  #   in_queue - optional boolean to indicate if we are in the queue code vs. UI/API
  #
  # Output:
  #   [ HTTP Error Code, String Message ]
  # CB1
  def proposal_commit(inst, in_queue = false)
    barclamp.deployment.first.commit
  end

  #
  # This can be overridden.  Specific to node validation.
  # proposal_elements is a hash from the elements field for the deployment/bc hash
  # CB1
  def validate_proposal_elements proposal_elements
    # moved to barclamp.is_valid? deployment
  end

  #
  # This can be overridden to get better validation if needed.
  # proposal is a hash in json proposal format
  #
  def validate_proposal proposal
    # moved to barclamp.is_valid? deployment
  end

  #
  # After validation, this is where the role is applied to the system
  # The old instance (if one exists) is compared with the new instance.
  # roles are removed and delete roles are added (if they exist) for nodes leaving roles
  # roles are added for nodes joining roles.
  # Calls chef-client on nodes
  #
  # This function can be overriden to define a barclamp specific operation.
  # A call is provided that receives the role and all string names of the nodes before the chef-client call
  #
  def apply_role(new_config, in_queue)
    Rails.logger.warn "Service object depricated MOVING TO JIG"
    #
    # Handled in ProposalQueue.queue_proposal
    #   Get dependents
    #     - if dependents aren't ready
    #        - Queue and mark as queued, return [202, "barclamps areno't ready"]
    #   Get nodes
    #     - if nodes aren't ready
    #        - Queue and mark as queued, return [202, "nodes aren't ready"]
    #
    queued, delay = ProposalQueue.get_queue("prop_queue", @logger).queue_proposal(new_config)
    return [202, delay] if queued

    # Save old active config and make this new config the active one.
    prop = new_config.proposal
    old_config = prop.active_config
    prop.active_config = new_config
    prop.save!

    # Put mark on the wall that we are committing
    ProposalQueue.update_proposal_status(new_config, ProposalConfig::STATUS_COMMITTING, "")

    #
    # Difference
    #   if node added to proposal
    #     - add role(states,priority) to node
    #   if node removed from proposal
    #     - add role_delete(states,priority) to node - if delete role exists
    #     - remove role(states,priority) to node
    #
    all_nodes = []
    new_role_nodes = new_config.get_nodes_by_roles
    old_role_nodes = old_config ? old_config.get_nodes_by_roles : {}
    @barclamp.roles.each do |role|
      added_nodes = (new_role_nodes[role.name] || []) - (old_role_nodes[role.name] || [])
      removed_nodes = (old_role_nodes[role.name] || []) - (new_role_nodes[role.name] || [])

      removed_nodes.each do |node|
        # Add the delete role if one exists, otherwise clear the config role
        drole = Role.find_by_name_and_barclamp_id("#{role.name}_remove", @barclamp.id)
        if drole
          add_role_to_instance_and_node(node.name, prop.name, drole.name)
        end
      end

      all_nodes << removed_nodes
      all_nodes << added_nodes
    end
    all_nodes.flatten.uniq.each do |node|
      node.update_run_list
    end

    #
    # Let the barclamp do some changes before chef-client runs
    #
    onodes = old_config ? old_config.nodes : []
    nnodes = new_config ? new_config.nodes : []
    all_nodes = (nnodes+onodes).uniq
    apply_role_pre_chef_call(old_config, new_config, all_nodes)
    all_nodes.each{|n|n.save}

    # CHEF ROLE OBJECT COMMAND - Make sure the chef object is up-to-date
    config_role = RoleObject.proposal_hash_to_role(prop.active_config.to_proposal_object_hash, @bc_name)

    # Each batch is a list of nodes that can be done in parallel.
    ran_admin = false
    @barclamp.get_roles_by_order.each do |role_list|
      nodes = role_list.collect { |role| (new_role_nodes[role.name]||[]) }.flatten.uniq
      next if nodes.empty?
      # Run chef-client on admin nodes last.
      nodes.partition{|n| !n.is_admin?}.each do |node_list|
        next if node_list.empty?
        pids = {}
        if node_list[0].is_admin?
          ran_admin = true
          chef_client = "/opt/dell/bin/single_chef_client.sh"
        else
          chef_client = "chef-client"
        end
        node_list.each do |node|
          filename = "log/#{node.name}.chef_client.log"
          pid = run_remote_chef_client(node.name, chef_client, filename)
          pids[pid] = node.name
        end
        status = Process.waitall
        badones = status.select { |x| x[1].exitstatus != 0 }
        next if badones.empty?
        badones.each do |baddie|
          node = pids[baddie[0]]
          @logger.warn("Re-running chef-client again for a failure: #{node} #{@bc_name}")
          filename = "log/#{node}.chef_client.log"
          pid = run_remote_chef_client(node, chef_client, filename)
          pids[pid] = node
        end
        status = Process.waitall
        badones = status.select { |x| x[1].exitstatus != 0 }
        next if badones.empty?
        message = "Failed to apply the proposal to: "
        badones.each do |baddie|
          message = message + "#{pids[baddie[0]]} "
        end
        ProposalQueue.update_proposal_status(new_config, ProposalConfig::STATUS_FAILED, message)
        ProposalQueue.restore_to_ready(all_nodes)
        q = ProposalQueue.get_queue('prop_queue', @logger)
        q.process_queue unless in_queue
        return [405, message]
      end
    end
    # XXX: This should not be done this way.  Something else should request this.
    system("sudo -i /opt/dell/bin/single_chef_client.sh") if !ran_admin

    ProposalQueue.update_proposal_status(new_config, ProposalConfig::STATUS_APPLIED, "")
    ProposalQueue.restore_to_ready(all_nodes)
    q = ProposalQueue.get_queue('prop_queue', @logger)
    q.process_queue unless in_queue
    [200, {}]
  end

  #
  # Override function to inject changes to nodes after role update, but before chef-client runs.
  #
  def apply_role_pre_chef_call(old_config, new_config, all_nodes)
    Rails.logger.warn "Service object depricated MOVING TO JIG"
    # noop by default.
  end

  #
  # Inputs: role = RoleObject of proposal being applied/queued.
  # Returns: List of hashs { "barclamp" => bcname, "inst" => instname }
  #
  def proposal_dependencies(role)
    Rails.logger.warn "Service object depricated MOVING TO BARCLAMP"
    # Default none
    []
  end

  #
  # XXX: Should this clone a new config?
  #
  # This is a helper function for places that need to add a node to a role in a proposal_config
  #
  def add_role_to_instance_and_node(node_name, prop_name, newrole)
    Rails.logger.warn "Service object depricated MOVING TO JIG"
    @logger.debug("ARTOI: enterin #{node_name}, #{prop_name}, #{newrole}")
    node = Node.find_by_name node_name    
    if node.nil?
      @logger.debug("ARTOI: couldn't find node #{node_name}. bailing")
      return false 
    end

    prop = @barclamp.get_proposal(prop_name)
    prop_config = prop.active? ? prop.active_config : prop.current_config
    if prop_config.nil?
      @logger.debug("ARTOI: couldn't find prop_config #{prop_name}. bailing")
      return false 
    end

    role = Role.find_by_name_and_barclamp_id(newrole, prop.barclamp.id)
    if role.nil?
      @logger.debug("ARTOI: couldn't find role #{newrole}. bailing")
      return false 
    end

    @logger.debug("ARTOI: add_node_to_role #{node}, #{role}")
    prop_config.add_node_to_role(node, role)

    # Update running config if active, otherwise apply/commit will catch it
    if prop.active?
      # Update Chef objects.
      raw_data = prop_config.to_proposal_object_hash
      ProposalObject.write(raw_data)
      ro = RoleObject.proposal_hash_to_role(raw_data, prop.barclamp.name)

      # Update Run list
      node.update_run_list
    end
      
    true
  end

  #
  # fork and exec ssh call to node and return pid.
  #
  def run_remote_chef_client(node, command, logfile_name)
    Rails.logger.warn "Service object depricated MOVING TO JIG"
    Kernel::fork {
      # Make sure all file descriptors are closed
      ObjectSpace.each_object(IO) do |io|
        unless [STDIN, STDOUT, STDERR].include?(io)
          begin
            unless io.closed?
              io.close
            end
          rescue ::Exception
          end
        end
      end

      # Fix the normal file descriptors.
      begin; STDIN.reopen "/dev/null"; rescue ::Exception; end       
      if logfile_name
        begin
          STDOUT.reopen logfile_name, "a+"
          File.chmod(0644, logfile_name)
          STDOUT.sync = true
        rescue ::Exception
          begin; STDOUT.reopen "/dev/null"; rescue ::Exception; end
        end
      else
        begin; STDOUT.reopen "/dev/null"; rescue ::Exception; end
      end
      begin; STDERR.reopen STDOUT; rescue ::Exception; end
      STDERR.sync = true      

      # Exec command
      # the -- tells sudo to stop interpreting options
      exec("sudo -i -u root -- ssh root@#{node} #{command}")
    }
  end


  def self.id?(object_id)
    Rails.logger.warn "Service object depricated"
    object_id = object_id.to_s
    object_id.match('^[0-9]+') ? true : false
  end


  def self.name?(object_id)
    Rails.logger.warn "Service object depricated"
    !self.id?(object_id)
  end


  def self.get_object(type, object_id)
    Rails.logger.warn "Service object depricated"
    object = nil
    object_id = object_id.to_s

    raise ActiveRecord::RecordNotFound, "object_id was not specified" if object_id.empty?

    if object_id.match('^[0-9]+')
      object = type.find(object_id)
    else
      objects = type.where( :name => object_id )
      if objects.size == 0
        raise ActiveRecord::RecordNotFound, "Unable to find #{type} with id=#{object_id}"
      end
      object = objects[0] if objects.size == 1
      if objects.size > 1
        raise "There are #{objects.size} #{type}s with the name #{object_id}"
      end
    end

    object
  end


  def self.get_object_safe(type, object_id)
    Rails.logger.warn "Service object depricated"
    begin
      return get_object(type, object_id)
    rescue ActiveRecord::RecordNotFound => ex
      return nil
    end
  end
end
