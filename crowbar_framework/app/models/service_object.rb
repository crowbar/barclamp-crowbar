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
# Author: RobHirschfeld 
# 
#
# Also functions as a data bag item wrapper as well.
#
require 'chef'
require 'json'

class ServiceObject
  extend CrowbarOffline

  def initialize(thelogger)
    @bc_name = "unknown"
    @logger = thelogger
  end

  def self.bc_name
    self.name.underscore[/(.*)_service$/,1]
  end
  
  def random_password(size = 12)
    chars = (('a'..'z').to_a + ('0'..'9').to_a) - %w(i o 0 1 l 0)
    (1..size).collect{|a| chars[rand(chars.size)] }.join
  end

  #
  # Locking Routines
  #
  def acquire_lock(name)
    @logger.debug("Acquire #{name} lock enter")
    f = File.new("tmp/#{name}.lock", File::RDWR|File::CREAT, 0644)
    @logger.debug("Acquiring #{name} lock")
    rc = false
    count = 0
    while rc == false do
      count = count + 1
      @logger.debug("Attempt #{name} Lock: #{count}")
      rc = f.flock(File::LOCK_EX|File::LOCK_NB)
      sleep 1 if rc == false
    end
    @logger.debug("Acquire #{name} lock exit: #{f.inspect}, #{rc}")
    f
  end

  def release_lock(f)
    @logger.debug("Release lock enter: #{f.inspect}")
    f.flock(File::LOCK_UN)
    f.close
    @logger.debug("Release lock exit")
  end

  def bc_name=(new_name)
    @bc_name = new_name
    @barclamp = Barclamp.find_by_name(@bc_name)
  end
  
  def bc_name 
    @bc_name
  end
  
#
# API Functions
#
  def transition
    [200, {}]
  end

  def destroy_active(prop_name)
    @logger.debug "Trying to deactivate role #{prop_name}" 
    prop = @barclamp.get_proposal(inst)
    return [404, {}] if prop.nil? or not prop.active?

    # Clear the nodes and apply the role.
    new_prop_config = prop.active_config.deep_clone
    new_prop_config.remove_all_nodes
    new_prop_config.save!

    # Apply new proposal_config
    answer = apply_role(new_prop_config, false)

    # Clear the active config
    prop.active_config = nil
    prop.save!

    # CHEF: Actual undo
    inst = "#{@bc_name}-config-#{prop_name}"
    role = RoleObject.find_role_by_name(inst)
    role.destroy

    answer
  end

  def dequeue_proposal(inst)
    prop = @barclamp.get_proposal(inst)
    return [404, {}] if prop.nil? or not prop.active? or not prop.active_config.queued?

    ret = ProposalQueue.get_queue('prop_queue').dequeue_proposal(inst, @bc_name)
    return [400, "error"] unless ret
    [200, {}]
  end

  #
  # Updates current_config
  # 
  def _proposal_update(new_prop, params)
    if params["attributes"]
      chash = new_prop.current_config.config_hash
      new_prop.current_config.config_hash = chash.merge(params["attributes"])
    end
    elems = params["deployment"][@bc_name]["elements"] rescue nil
    new_prop.current_config.update_node_roles(elems) if elems

    raw_data = proposal.current_config.to_proposal_object_hash
    validate_proposal raw_data

    # Write out proposal object.
    return ProposalObject.write(raw_data)
  end

  #
  # This can be overridden to provide a better creation proposal
  #
  def create_proposal
    @barclamp.create_proposal
  end

  def proposal_create(params)
    base_id = params["id"]

    prop = @barclamp.get_proposal(base_id)
    return [400, I18n.t('model.service.name_exists')] unless prop.nil?
    return [400, I18n.t('model.service.too_short')] if base_id.length == 0
    return [400, I18n.t('model.service.illegal_chars', :name => base_id)] if base_id =~ /[^A-Za-z0-9_]/

    new_prop = create_proposal
    new_prop.name = base_id
    new_prop.save!

    _proposal_update new_prop, params
  end

  def proposal_edit(params)
    proposal = @barclamp.get_proposal(params["id"])
    return [404, I18n.t('model.service.cannot_find')] if prop.nil?

    # Make copy of config for history.
    new_prop_config = proposal.current_config.deep_clone
    proposal.current_config = new_prop_config
    proposal.save!

    _proposal_update proposal, params
  end

  def proposal_delete(inst)
    prop = @barclamp.get_proposal(inst)
    if prop.nil?
      [404, I18n.t('model.service.cannot_find')]
    elsif prop.active?
      [400, I18n.t('model.service.instance_active')]
    else
      success = @barclamp.delete_proposal(prop)
      [200, {}] if success
      [400, I18n.t('model.service.unknown_delete_fail')] unless success
    end
  end


  def proposal_commit(inst, in_queue = false)
    prop = @barclamp.get_proposal(inst)
    if prop.nil?
      [404, "#{I18n.t('.cannot_find', :scope=>'model.service')}: #{@bc_name}.#{inst}"]
    elsif prop.active? and prop.active_config.committing?
      [402, "#{I18n.t('.already_commit', :scope=>'model.service')}: #{@bc_name}.#{inst}"]
    else
      begin
        apply_role(prop.current_config, in_queue)
      rescue Net::HTTPServerException => e
        [e.response.code, e.message]
      rescue Chef::Exceptions::ValidationFailed => e2
        [400, e2.message]
      end
    end
  end

  #
  # This can be overridden.  Specific to node validation.
  # proposal_elements is a hash from the elements field for the deployment/bc hash
  #
  def validate_proposal_elements proposal_elements
    proposal_elements.each do |role_and_elements|
      elements = role_and_elements[1]
      uniq_elements = elements.uniq
      if uniq_elements.length != elements.length
        raise  I18n.t('proposal.failures.duplicate_elements_in_role')+" "+role_and_elements[0]
      end
      uniq_elements.each do |node_name|
        nodes = NodeObject.find_nodes_by_name node_name
        if 0 == nodes.length
          raise  I18n.t('proposal.failures.unknown_node')+" "+node_name
        end
      end
    end
  end

  #
  # This can be overridden to get better validation if needed.
  # proposal is a hash in json proposal format
  #
  def validate_proposal proposal
    path = "/opt/dell/chef/data_bags/crowbar"
    path = "schema" unless CHEF_ONLINE
    validator = CrowbarValidator.new("#{path}/bc-template-#{@bc_name}.schema")
    Rails.logger.info "validating proposal #{@bc_name}"

    errors = validator.validate(proposal)
    if errors && !errors.empty?
      strerrors = ""
      errors.each do |e|
        strerrors += "#{e.message}\n"
      end
      Rails.logger.info "validation errors in proposal #{@bc_name}"
      raise Chef::Exceptions::ValidationFailed.new(strerrors)
    end
    validate_proposal_elements proposal['deployment'][@bc_name]["elements"]
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
    ProposalQueue.update_proposal_status(new_config, ProposalConfig::COMMITTING, "")

    # CHEF ROLE OBJECT COMMAND
    config_role = RoleObject.proposal_hash_to_role(prop.active_config.to_proposal_object_hash, @bc_name)
    config_role_name = config_role.name

    #
    # Difference
    #   if node added to proposal
    #     - add role(states,priority) to node
    #   if node removed from proposal
    #     - add role_delete(states,priority) to node - if delete role exists
    #     - remove role(states,priority) to node
    #
    new_role_nodes = new_config.get_nodes_by_role
    old_role_nodes = old_config ? old_config.get_nodes_by_role : {}
    @barclamp.roles.each do |role|
      added_nodes = new_config - old_config
      removed_nodes = old_config - new_config

      removed_nodes.each do |node|
        node.delete_from_run_list role.name
        # Add the delete role if one exists, otherwise clear the config role
        drole = Role.find_by_name_and_barclamp_id("#{role.name}_remove", @barclamp.id)
        if drole
          node.add_to_run_list(drole.name, @barclamp.cmdb_order, drole.states)
        else 
          node.delete_from_run_list config_role_name
        end
        node.save!
      end

      added_nodes.each do |node|
        node.add_to_run_list(role.name, @barclamp.cmdb_order, role.states)
        node.add_to_run_list(config_role_name, @barclamp.cmdb_order, role.states)
        node.save!
      end
    end

    apply_role_pre_chef_call(old_config, new_config, (new_config.nodes + old_config.nodes).uniq)

    # Each batch is a list of nodes that can be done in parallel.
    ran_admin = false
    @barclamp.roles_by_order.each do |role_list|
      nodes = role_list.collect { |role| new_role_nodes[role.name] }.flatten.uniq
      next if nodes.empty?
      snodes = []
      admin_list = []
      batch.each do |n|
        # Run admin nodes a different way.
        if node.is_admin?
          ran_admin = true
          admin_list << node
        else 
          snodes << n
        end
      end
 
      @logger.debug("AR: Calling knife for #{role.name} on non-admin nodes #{snodes.join(" ")}")
      @logger.debug("AR: Calling knife for #{role.name} on admin nodes #{admin_list.join(" ")}")

      # Only take the actions if we are online
      if CHEF_ONLINE
        # 
        # XXX: We used to do this twice - do we really need twice???
        # Yes! We do!  The system has some transient issues that are hidden
        # but the double run for failing nodes.  For now, we will do this.
        # Make this better one day.
        #
        pids = {}
        unless snodes.empty?
          snodes.each do |node|
            filename = "log/#{node.name}.chef_client.log"
            pid = run_remote_chef_client(node.name, "chef-client", filename)
            pids[pid] = node.name
          end
          status = Process.waitall
          badones = status.select { |x| x[1].exitstatus != 0 }

          unless badones.empty?
            badones.each do |baddie|
              node = pids[baddie[0]]
              @logger.warn("Re-running chef-client again for a failure: #{node} #{@bc_name} #{inst}")
              filename = "log/#{node}.chef_client.log"
              pid = run_remote_chef_client(node, "chef-client", filename)
              pids[pid] = node
            end
            status = Process.waitall
            badones = status.select { |x| x[1].exitstatus != 0 }

            unless badones.empty?
              message = "Failed to apply the proposal to: "
              badones.each do |baddie|
                message = message + "#{pids[baddie[0]]} "
              end
              ProposalQueue.update_proposal_status(new_config, ProposalConfig::FAILED, message)
              ProposalQueue.restore_to_ready(all_nodes)
              ProposalQueue.get_queue('prop_queue').process_queue unless in_queue
              return [ 405, message ] 
            end
          end
        end

        unless admin_list.empty?
          admin_list.each do |node|
            filename = "log/#{node.name}.chef_client.log"
            pid = run_remote_chef_client(node.name, "/opt/dell/bin/single_chef_client.sh", filename)
            pids[pid] = node.name
          end
          status = Process.waitall
          badones = status.select { |x| x[1].exitstatus != 0 }

          unless badones.empty?
            badones.each do |baddie|
              node = pids[baddie[0]]
              @logger.warn("Re-running chef-client (admin) again for a failure: #{node} #{@bc_name} #{inst}")
              filename = "log/#{node}.chef_client.log"
              pid = run_remote_chef_client(node, "/opt/dell/bin/single_chef_client.sh", filename)
              pids[pid] = node
            end
            status = Process.waitall
            badones = status.select { |x| x[1].exitstatus != 0 }

            unless badones.empty?
              message = "Failed to apply the proposal to: "
              badones.each do |baddie|
                message = message + "#{pids[baddie[0]]} "
              end
              ProposalQueue.update_proposal_status(new_config, ProposalConfig::FAILED, message)
              ProposalQueue.restore_to_ready(all_nodes)
              ProposalQueue.get_queue('prop_queue').process_queue unless in_queue
              return [ 405, message ] 
            end
          end
        end
      end
    end

    # XXX: This should not be done this way.  Something else should request this.
    system("sudo -i /opt/dell/bin/single_chef_client.sh") if CHEF_ONLINE and !ran_admin

    ProposalQueue.update_proposal_status(new_config, ProposalConfig::APPLIED, "")
    ProposalQueue.restore_to_ready(all_nodes)
    ProposalQueue.get_queue('prop_queue').process_queue unless in_queue
    [200, {}]
  end

  def apply_role_pre_chef_call(old_role, role, all_nodes)
    # noop by default.
  end

  #
  # Inputs: role = RoleObject of proposal being applied/queued.
  # Returns: List of hashs { "barclamp" => bcname, "inst" => instname }
  #
  def proposal_dependencies(role)
    # Default none
    []
  end

  #
  # XXX: Should this clone a new config?
  #
  def add_role_to_instance_and_node(node_name, prop, newrole)
    node = Node.find_by_name node_name    
    if node.nil?
      @logger.debug("ARTOI: couldn't find node #{name}. bailing")
      return false 
    end

    prop_config = prop.active? ? prop.active_config : prop.current_config
    if prop_config.nil?
      @logger.debug("ARTOI: couldn't find prop_config #{prop.name}. bailing")
      return false 
    end

    role = Role.find_by_name_and_barclamp_id(newrole, prop.barclamp.id)
    if role.nil?
      @logger.debug("ARTOI: couldn't find role #{newrole}. bailing")
      return false 
    end
    prop_config.add_node_to_role(node, role)

    # Update Chef objects.
    raw_data = prop_config.to_proposal_object_hash
    ProposalObject.write(raw_data)
    ro = RoleObject.proposal_hash_to_role(raw_data, prop.barclamp.name)

    # Update Run list
    node.add_to_run_list(role.name, prop.barclamp.cmdb_order, role.states)
    node.add_to_run_list(ro.name, prop.barclamp.cmdb_order, role.states)
    node.save!

    true
  end

  #
  # fork and exec ssh call to node and return pid.
  #
  def run_remote_chef_client(node, command, logfile_name)
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


end

