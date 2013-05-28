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

class CrowbarService < ServiceObject
  
  #
  # Below are the parts to handle transition requests.
  #
  # This routine handles name-based state transitions.  The system will then inform barclamps.
  # It will create a node and assign it an admin address.
  #
  def transition(inst, name, state)
    save_it = false

    return [404, "No state specified"] if state.nil?
    # FIXME: validate state

    @logger.info("Crowbar transition enter: #{name} to #{state}")

    f = acquire_lock "BA-LOCK"
    begin
      node = NodeObject.find_node_by_name name
      if node.nil? and (state == "discovering" or state == "testing")
        @logger.debug("Crowbar transition: creating new node for #{name} to #{state}")
        node = NodeObject.create_new name
      end
      if node.nil?
        @logger.error("Crowbar transition leaving: node not found nor created - #{name} to #{state}")
        return [404, "Node not found"]
      end

      node.crowbar["crowbar"] = {} if node.crowbar["crowbar"].nil?
      node.crowbar["crowbar"]["network"] = {} if node.crowbar["crowbar"]["network"].nil?

      if state == "discovering" and node.allocated.nil?
        @logger.debug("Crowbar transition: marking #{name} as initially not allocated")
        node.allocated = false
        save_it = true
      end

      pop_it = false
      if (state == "hardware-installing" or state == "hardware-updating" or state == "update") 
        @logger.debug("Crowbar transition: force run because of state #{name} to #{state}")
        pop_it = true
      end

      if node.crowbar["state"] != state
        @logger.debug("Crowbar transition: state has changed so we need to do stuff for #{name} to #{state}")

        node.crowbar["crowbar"]["state_debug"] = {} if node.crowbar["crowbar"]["state_debug"].nil?
        if node.crowbar["crowbar"]["state_debug"][state].nil?
          node.crowbar["crowbar"]["state_debug"][state] = 1
        else
          node.crowbar["crowbar"]["state_debug"][state] = node.crowbar["crowbar"]["state_debug"][state] + 1
        end

        node.crowbar["state"] = state
        node.crowbar["state_change_time"] = Time.new.to_s
        save_it = true
        pop_it = true
      end
    ensure
      release_lock f
    end

    node.save if save_it

    if pop_it
      #
      # If we are discovering the node and it is an admin, 
      # make sure that we add the crowbar config
      #
      if state == "discovering" and node.admin?
        crole = RoleObject.find_role_by_name("crowbar-config-#{inst}")
        db = ProposalObject.find_proposal("crowbar", inst)
        add_role_to_instance_and_node("crowbar", inst, name, db, crole, "crowbar")
      end

      catalog = ServiceObject.barclamp_catalog
      roles = RoleObject.find_roles_by_search "transitions:true AND (transition_list:all OR transition_list:#{ChefObject.chef_escape(state)})"
      # Sort rules for transition order (deployer should be near the beginning if not first).
      roles.sort! do |x,y| 
        xname = x.name.gsub(/-config-.*$/, "")
        yname = y.name.gsub(/-config-.*$/, "")

        xs = ServiceObject.run_order(xname, catalog)
        ys = ServiceObject.run_order(yname, catalog)
        xs <=> ys
      end

      roles.each do |role|
        role.override_attributes.each do |bc, data|
          jsondata = {
            "name" => name,
            "state" => state
          }
          rname = role.name.gsub("#{bc}-config-","")
          # Need a lock here, because if many nodes are discovered
          # simultaneously, adding them to a proposal can race,
          # leaving some nodes not present in proposals. e.g.:
          # NtpService::transition uses find_proposal and
          # later saves it with add_role_to_instance_and_node().
          # If this runs for two nodes at the same time, they both
          # find the proposal, then both modify it, then both save
          # it in lockstep.  Naturally the second save clobbers
          # the first, so the first node won't be present in that
          # proposal.
          bc_lock = acquire_lock "#{bc}:#{rname}"
          begin
            svc_name = "#{bc.camelize}Service"
            @logger.info("Crowbar transition: calling #{bc}:#{rname} for #{name} for #{state} - svc: #{svc_name}")            
            service = eval("#{svc_name}.new @logger")
            answer = service.transition(rname, name, state)
            if answer[0] != 200
              @logger.error("Crowbar transition: finished #{bc}:#{rname} for #{name} for #{state}: FAILED #{answer[1]}")
            else
              @logger.debug("Crowbar transition: finished #{bc}:#{rname} for #{name} for #{state}")
              unless answer[1]["name"].nil?
                name = answer[1]["name"]
              end
            end
          rescue Exception => e
            @logger.fatal("json/transition for #{bc}:#{rname} failed: #{e.message}")
            @logger.fatal("#{e.backtrace.join("\n")}")
            return [500, "#{bc} transition to #{rname} failed.\n#{e.message}\n#{e.backtrace.join("\n")}"]
          ensure
            release_lock bc_lock
          end
        end
      end

      # The node is going to call chef-client on return or as a side-effect of the process queue.
      node = NodeObject.find_node_by_name(name)
      node.rebuild_run_list
      node.save

      # We have a node that has become ready, test to see if there are queued proposals to commit
      process_queue if state == "ready"
    end

    @logger.debug("Crowbar transition leaving: #{name} to #{state}")
    [200, NodeObject.find_node_by_name(name).to_hash ]
  end

  def create_proposal
    @logger.debug("Crowbar create_proposal enter")
    base = super
    @logger.debug("Crowbar create_proposal exit")
    base
  end

  def apply_role (role, inst, in_queue)
    @logger.debug("Crowbar apply_role: enter")
    answer = super
    @logger.debug("Crowbar apply_role: super apply_role finished")

    role = role.default_attributes
    @logger.debug("Crowbar apply_role: create initial instances")
    unless role["crowbar"].nil? or role["crowbar"]["instances"].nil?
      ordered_bcs = order_instances role["crowbar"]["instances"]
#      role["crowbar"]["instances"].each do |k,plist|
      ordered_bcs.each do |k, plist |
        @logger.fatal("Deploying proposal - id: #{id}, name: #{plist[:instances].join(',')}")
        plist[:instances].each do |v|
          id = "default"
          data = "{\"id\":\"#{id}\"}" 
          @logger.fatal("Deploying proposal - id: #{id}, name: #{v.inspect}")

          if v != "default"
            file = File.open(v, "r")
            data = file.readlines.to_s
            file.close

            struct = JSON.parse(data)
            id = struct["id"].gsub("bc-#{k}-", "")
          end

          @logger.debug("Crowbar apply_role: creating #{k}.#{id}")

          # Create a service to talk to.
          service = eval("#{k.camelize}Service.new @logger")

          @logger.debug("Crowbar apply_role: Calling get to see if it already exists: #{k}.#{id}")
          answer = service.proposals
          if answer[0] != 200
            @logger.error("Failed to list #{k}: #{answer[0]} : #{answer[1]}")
          else
            unless answer[1].include?(id)
              @logger.debug("Crowbar apply_role: didn't already exist, creating proposal for #{k}.#{id}")
              answer = service.proposal_create JSON.parse(data)
              if answer[0] != 200
                answer[1] = "Failed to create proposal '#{id}' for barclamp '#{k}' " +
                            "(The error message was: #{answer[1].strip})"
                break
              end
            end

            @logger.debug("Crowbar apply_role: check to see if it is already active: #{k}.#{id}")
            answer = service.list_active
            if answer[0] != 200
              answer[1] = "Failed to list active '#{k}' proposals " +
                          "(The error message was: #{answer[1].strip})"
              break
            else
              unless answer[1].include?(id)
                @logger.debug("Crowbar apply_role: #{k}.#{id} wasn't active: Activating")
                answer = service.proposal_commit id
                if answer[0] != 200
                  answer[1] = "Failed to commit proposal '#{id}' for '#{k}' " +
                              "(The error message was: #{answer[1].strip})"
                  break
                end
              end
            end
          end

          @logger.fatal("Crowbar apply_role: Done with creating: #{k}.#{id}")
        end
        if answer[0] != 200
          break
        end
      end
    end

    if answer[0] != 200
      @logger.error("Crowbar apply_role: #{answer.inspect}")
    else
      @logger.debug("Crowbar apply_role: leaving: #{answer.inspect}")
    end
    answer
  end

  # look at the instances we'll create, and sort them using catalog info
  def order_instances(bcs)
    tmp = {}
    bcs.each { |bc_name,instances|
      order = ServiceObject.run_order(bc_name)
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
    proposals = ProposalObject.find_proposals("crowbar")
    raise "Can't find any crowbar proposal" if proposals.nil? or proposals[0].nil?
    # populate options from attributes/crowbar/*-settings
    options = { :raid=>{}, :bios=>{}, :show=>[] }
    unless proposals[0]["attributes"].nil? or proposals[0]["attributes"]["crowbar"].nil?
      options[:raid] = proposals[0]["attributes"]["crowbar"]["raid-settings"]
      options[:bios] = proposals[0]["attributes"]["crowbar"]["bios-settings"]
      options[:show] << :raid if options[:raid].length > 0
      options[:show] << :bios if options[:bios].length > 0
    end
    options
  end

end

