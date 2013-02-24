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

class CrowbarService < ServiceObject
  
  #
  # Transition function for the crowbar barclamp
  #
  # This is the main entry function into crowbar.
  #
  # Input:
  #   inst = Name of the instance of crowbar to operation on ("default")
  #   name = Name of node being transitioned
  #   state = State node should transition to
  #   old_state = optional parameter that should be used as test and set.
  #
  # Output:
  #   [ HTTP Return Code (200 success, ...), Message for Failure ]
  #
  # The Crowbar Service is the main entry point into the whole transition system.
  #
  # General Flow:
  # Under the BA-LOCK
  #   If the state requested is discovering or testing, the node will be created if it 
  #     doesn't exist.  
  #   If the node doesn't exist, return a 404
  #   Update the state on the node.
  #
  # If the node is the admin and we are discovering, make sure we have the crowbar role on the node
  #
  # If the state changes or is one of hardware-installing, hardware-updating, or update,
  #   we need to the following:
  #     find all the barclamp proposals that care that have registered to be called for this state
  #     Order the proposals by their barclamp run_order
  #     Call each proposal's transition function
  #     Rebuild the Jig structures for the node
  #     If the node goes ready, check to see if we can apply any queued proposals.
  #
  # Return success
  #
  def transition(inst, name, state, old_state = nil)
    @logger.info("Crowbar transition enter: #{name} to #{state}")
    node = nil
    CrowbarUtils.with_lock("BA-LOCK-#{name}") do
      node = Node.find_by_name name
      if node.nil? && ['discovering','testing'].member?(state)
        @logger.debug("Crowbar transition: creating new node for #{name} to #{state}")
        node = Node.create_with_jig(name)
      end
      if node.nil?
        @logger.error("Crowbar transition leaving: chef node not found nor created - #{name} to #{state}")
        return [404, "Node not found"] # GREG: Translate
      end
      if old_state and old_state != node.state
        @logger.error("Crowbar transition leaving: node #{name} expected to be #{old_state} but was at #{node.state}")
        return [409, "Node not in correct state: #{node.state} #{old_state}"] # GREG: Translate
      end
      unless (node.state != state) ||
          ['hardware-installing','hardware-updating','update'].member?(state)
        @logger.info("Crowbar transition: no state transition needed for #{name}")
        return [200,node.jig_hash]
      end
      node.state = state
      node.save
    end

    #
    # If we are discovering the node and it is an admin, 
    # make sure that we add the crowbar config
    #
    if state == "discovering" and node.is_admin?
      add_role_to_instance_and_node(name, inst, "crowbar")
    end
    
    # Find the active proposals that have this as a transition state
    props = []
    Barclamp.all.each do |x| 
      if x.transitions
        states = x.transition_list.split(",")
        props << x.active_proposals if states.include?(state) or states.include?("all")
      end
    end
    props.flatten.sort{|x,y|x.barclamp.run_order <=> y.barclamp.run_order}.each do |prop|
      bco = prop.barclamp
      begin
        @logger.info("Crowbar transition: calling #{bco.name}:#{prop.name} for #{name} for #{state}") 
        answer = bco.operations(@logger).transition(prop.name, name, state)
        if answer[0] != 200
          @logger.error("Crowbar transition: finished #{bco.name}:#{prop.name} for #{name} for #{state}: FAILED #{answer[1]}")
        else
          @logger.debug("Crowbar transition: finished #{bco.name}:#{prop.name} for #{name} for #{state}")
        end
      rescue Exception => e
        @logger.fatal("json/transition for #{bco.name}:#{prop.name} failed: #{e.message}")
        @logger.fatal("#{e.backtrace}")
        return [500, e.message]
      end
    end
    #
    # The temp booting images need to have clients cleared.
    #
    node.reset_jig_access
    if state == "delete"
      @logger.info("Crowbar: Deleting #{name}")
      node.destroy
      [200, "#{name} deleted" ]
    end

    # Process any queued proposals if the queue lock is not held,
    # and the node transitioned to ready.
    if !CrowbarUtils.lock_held?("queue") && (state == "ready")
      ProposalQueue.get_queue('prop_queue', @logger).process_queue
    end
    @logger.debug("Crowbar transition leaving: #{name} to #{state}")
    [200, node.jig_hash]
  end

  #
  # apply_role
  # Input:
  #   role = proposal_config object that is being applied to the system
  #   in_queue = boolean to indicate if this is being called from the queuing system
  #
  # Output:
  #   [ HTTP Error Code, String Message ]
  #
  # The Crowbar barclamp apply_role overrides the default apply path to enable
  # the system to create and commit the initial barclamp instances as defined in the
  # proposal_config's data.  
  #
  # The override calls the parent function to get this applied, but then follows that with
  # a set of create/commit calls for each instance in the deployment.  This usually
  # creates the default deployed barclamps.
  #
  def apply_role (role, in_queue)
    @logger.debug("Crowbar apply_role: enter")
    answer = super(role, in_queue)
    @logger.debug("Crowbar apply_role: super apply_role finished")

    role = role.config_hash
    @logger.debug("Crowbar apply_role: create initial instances")
    if role and role["crowbar"] and role["crowbar"]["instances"]
      ordered_bcs = order_instances role["crowbar"]["instances"]
      ordered_bcs.each do |k, plist |
        @logger.fatal("Deploying proposals - id: #{k}, name: #{plist[:instances].join(',')}")
        plist[:instances].each do |v|
          prop_id = "default"
          data = "{\"id\":\"#{prop_id}\"}" 
          @logger.fatal("Deploying proposal - id: #{prop_id}, name: #{v.inspect}")

          if v != "default"
            file = File.open(v, "r")
            data = file.readlines.to_s
            file.close

            struct = JSON.parse(data)
            prop_id = struct["id"].gsub("bc-#{k}-", "")
          end

          @logger.debug("Crowbar apply_role: creating #{k}.#{prop_id}")

          # Create a service to talk to.
          barclamp = Barclamp.find_by_name(k)

          @logger.debug("Crowbar apply_role: Calling get to see if it already exists: #{k}.#{prop_id}")
          prop = barclamp.get_proposal(prop_id)
          unless prop
            @logger.debug("Crowbar apply_role: didn't already exist, creating proposal for #{k}.#{prop_id}")
            answer = barclamp.operations(@logger).proposal_create JSON.parse(data)
            if answer[0] != 200
              @logger.error("Failed to create #{k}.#{prop_id}: #{answer[0]} : #{answer[1]}")
            end
          end
 
          @logger.debug("Crowbar apply_role: check to see if it is already active: #{k}.#{prop_id}")
          prop = barclamp.get_proposal(prop_id)
          unless prop.active?
            @logger.debug("Crowbar apply_role: #{k}.#{prop_id} wasn't active: Activating")
            answer = barclamp.operations(@logger).proposal_commit prop_id
            if answer[0] != 200
              @logger.error("Failed to commit #{k}.#{prop_id}: #{answer[0]} : #{answer[1]}")
            end
          end

          @logger.fatal("Crowbar apply_role: Done with creating: #{k}.#{prop_id}")
        end
      end
    end

    @logger.debug("Crowbar apply_role: leaving: #{answer}")
    answer
  end

  #
  # Helper function to order the pending instances so that
  # they get created in a specific order.
  #
  def order_instances(bcs)
    tmp = {}
    bcs.each { |bc_name,instances|
      order = Barclamp.find_by_name(bc_name).run_order rescue 1000
      tmp[bc_name] = {:order =>order, :instances =>instances}
    }
    #sort by the order value (x,y are an array with the value of
    #the hash entry
    t = tmp.sort{ |x,y| x[1][:order] <=> y[1][:order] } 
    @logger.fatal("ordered instances: #{t.inspect}")
    t
  end 

  #
  # Helper function to populate the UI RAID and BIOS toggles
  #
  # NOTE: HARDCODED TO DEFAULT proposal.
  #
  def self.read_options
    # read in default proposal, to make some vaules avilable
    bc = Barclamp.find_by_name("crowbar")

    proposals = (bc.get_proposal('default').nil?) ? bc.get_proposal('template') : bc.get_proposal('default')

    raise "Can't find any crowbar proposal" if proposals.nil?
    # populate options from attributes/crowbar/*-settings
    options = { :raid=>{}, :bios=>{}, :show=>[] }
    hash = proposals.current_config.config_hash
    if hash and hash["crowbar"]
      options[:raid] = hash["crowbar"]["raid-settings"]
      options[:bios] = hash["crowbar"]["bios-settings"]
      options[:show] << :raid if !options[:raid].nil? && options[:raid].length > 0
      options[:show] << :bios if !options[:bios].nil? && options[:bios].length > 0
    end
    options
  end

end

