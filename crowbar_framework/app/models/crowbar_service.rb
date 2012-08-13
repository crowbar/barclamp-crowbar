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
  # Below are the parts to handle transition requests.
  #
  # This routine handles name-based state transitions.  The system will then inform barclamps.
  # It will create a node and assign it an admin address.
  #
  def transition(inst, name, state)
    @logger.info("Crowbar transition enter: #{name} to #{state}")

    f = acquire_lock "BA-LOCK"
    begin
      node = Node.find_by_name name
      if node.nil? and (state == "discovering" or state == "testing")
        @logger.debug("Crowbar transition: creating new node for #{name} to #{state}")
        chef_node = NodeObject.find_node_by_name(name)
        node = Node.create(:name => name)
        node.admin = true if chef_node and chef_node.admin?
        node.save!
        unless chef_node
          cno = NodeObject.create_new name
          cno.crowbar["crowbar"] = {} if chef_node.crowbar["crowbar"].nil?
          cno.crowbar["crowbar"]["network"] = {} if chef_node.crowbar["crowbar"]["network"].nil?
          cno.save
        end
      end

      if node.nil?
        @logger.error("Crowbar transition leaving: chef node not found nor created - #{name} to #{state}")
        return [404, "Node not found"] # GREG: Translate
      end

      pop_it = false
      if (state == "hardware-installing" or state == "hardware-updating" or state == "update") 
        @logger.debug("Crowbar transition: force run because of state #{name} to #{state}")
        pop_it = true
      end

      if node.state != state
        @logger.debug("Crowbar transition: state has changed so we need to do stuff for #{name} to #{state}")

        node.state = state
        node.save
        pop_it = true
      end
    ensure
      release_lock f
    end

    if pop_it
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
          props << x.active_proposals if states.include?(state)
        end
      end
      props = props.flatten

      # Sort rules for transition order (deployer should be near the beginning if not first).
      props.sort! { |x,y| x.barclamp.run_order <=> y.barclamp.run_order }

      # For each prop, call the transition function.
      props.each do |prop|
        bco = prop.barclamp
        begin
          @logger.info("Crowbar transition: calling #{bco.name}:#{prop.name} for #{name} for #{state}")            
          answer = bco.operations(@logger).transition(prop.name, name, state)
          if answer[0] != 200
            @logger.error("Crowbar transition: finished #{bco.name}:#{prop.name} for #{name} for #{state}: FAILED #{answer[1]}")
          else
            @logger.debug("Crowbar transition: finished #{bco.name}:#{prop.name} for #{name} for #{state}")
            unless answer[1]["name"].nil?
              name = answer[1]["name"]
            end
          end
        rescue Exception => e
          @logger.fatal("json/transition for #{bco.name}:#{prop.name} failed: #{e.message}")
          @logger.fatal("#{e.backtrace}")
          return [500, e.message]
        end
      end

      # GREG: THIS MAY NOT BE NEEDED
      # The node is going to call chef-client on return or as a side-effet of the proces queue.
      chef_node = NodeObject.find_node_by_name(name)
      chef_node.rebuild_run_list
      chef_node.save

      # We have a node that has become ready, test to see if there are queued proposals to commit
      ProposalQueue.get_queue('prop_queue', @logger).process_queue if state == "ready"
    end

    @logger.debug("Crowbar transition leaving: #{name} to #{state}")
    [200, ""]
  end

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

  # look at the instances we'll create, and sort them 
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

  def self.read_options
    # read in default proposal, to make some vaules avilable
    proposals = Barclamp.find_by_name("crowbar").get_proposal('default')
    raise "Can't find any crowbar proposal" if proposals.nil?
    # populate options from attributes/crowbar/*-settings
    options = { :raid=>{}, :bios=>{}, :show=>[] }
    hash = proposals.current_config.config_hash
    if hash
      options[:raid] = hash["raid-settings"]
      options[:bios] = hash["bios-settings"]
      options[:show] << :raid if options[:raid].length > 0
      options[:show] << :bios if options[:bios].length > 0
    end
    options
  end

end

