#
# Copyright 2011-2013, Dell
# Copyright 2013-2014, SUSE LINUX Products GmbH
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

class CrowbarService < ServiceObject
  attr_accessor :transition_save_node

  def initialize(thelogger)
    super(thelogger)
    @bc_name = "crowbar"
  end

  class << self
    def role_constraints
      {
        "crowbar" => {
          "count" => 1,
          "admin" => true,
          "exclude_platform" => {
            "suse" => "12.0",
            "windows" => "/.*/"
          }
        },
        "crowbar-upgrade" => {
          "unique" => false,
          "count" => -1,
          "admin" => false,
          "exclude_platform" => {
            "windows" => "/.*/"
          }
        }
      }
    end
  end

  #
  # Below are the parts to handle transition requests.
  #
  # This routine handles name-based state transitions.  The system will then inform barclamps.
  # It will create a node and assign it an admin address.
  #
  def transition(inst, name, state)
    self.transition_save_node = false

    return [404, "No state specified"] if state.nil?
    # FIXME: validate state

    @logger.info("Crowbar transition enter: #{name} to #{state}")

    pop_it = false
    node = nil

    with_lock "BA-LOCK" do
      node = NodeObject.find_node_by_name name
      if node.nil? and (state == "discovering" or state == "testing")
        @logger.debug("Crowbar transition: creating new node for #{name} to #{state}")
        node = NodeObject.create_new name
        self.transition_save_node = true
      end
      if node.nil?
        @logger.error("Crowbar transition leaving: node not found nor created - #{name} to #{state}")
        return [404, "Node not found"]
      end

      if state == "readying"
        transition_to_readying inst, name, state, node
      end

      if %w(hardware-installing hardware-updating update).include? state
        @logger.debug("Crowbar transition: force run because of state #{name} to #{state}")
        pop_it = true
      end

      if node.crowbar["state"] != state
        @logger.debug("Crowbar transition: state has changed so we need to do stuff for #{name} to #{state}")

        # Do not allow change to shutdown state from crowbar_upgrade
        # (we need to reboot the nodes for upgrade, but without changing the state)
        if node.crowbar["state"] == "crowbar_upgrade" && state == "shutdown"
          @logger.debug("current node state is crowbar_upgrade; ignoring change to shutdown")
          return [200, { name: name }]
        end

        node.crowbar["crowbar"]["state_debug"] = {} if node.crowbar["crowbar"]["state_debug"].nil?
        if node.crowbar["crowbar"]["state_debug"][state].nil?
          node.crowbar["crowbar"]["state_debug"][state] = 1
        else
          node.crowbar["crowbar"]["state_debug"][state] = node.crowbar["crowbar"]["state_debug"][state] + 1
        end

        node.crowbar["state"] = state
        node.crowbar["state_change_time"] = Time.new.to_s
        self.transition_save_node = true
        pop_it = true
      end

      node.save if transition_save_node
    end

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

      roles = RoleObject.find_roles_by_search "transitions:true AND (transition_list:all OR transition_list:#{ChefObject.chef_escape(state)})"
      # Sort rules for transition order (deployer should be near the beginning if not first).
      roles.sort! do |x,y|
        xname = x.name.gsub(/-config-.*$/, "")
        yname = y.name.gsub(/-config-.*$/, "")

        xs = BarclampCatalog.run_order(xname)
        ys = BarclampCatalog.run_order(yname)
        xs <=> ys
      end

      roles.each do |role|
        role.override_attributes.each do |bc, data|
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
          rescue StandardError => e
            @logger.fatal("json/transition for #{bc}:#{rname} failed: #{e.message}")
            @logger.fatal("#{e.backtrace.join("\n")}")
            return [500, "#{bc} transition to #{rname} failed.\n#{e.message}\n#{e.backtrace.join("\n")}"]
          ensure
            bc_lock.release
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
    [200, { :name => name } ]
  end

  def create_proposal
    @logger.debug("Crowbar create_proposal enter")
    base = super
    @logger.debug("Crowbar create_proposal exit")
    base
  end

  def prepare_nodes_for_crowbar_upgrade
    proposal = ProposalObject.find_proposal('crowbar', 'default')

    # To all nodes, add a new role which prepares them for the upgrade
    nodes_to_upgrade = []
    not_ready_for_upgrade = []
    all_nodes = NodeObject.all
    all_nodes.each do |n|
      not_ready_for_upgrade.push n.name if !n.admin? && !%w(ready crowbar_upgrade).include?(n.state)
    end

    unless not_ready_for_upgrade.empty?
      raise I18n.t("upgrade.upgrade.nodes_not_ready", :nodes => not_ready_for_upgrade.join(', '))
    end

    all_nodes.each do |node|
      next if node.admin?

      if node[:platform] == "windows"
        # for Hyper-V nodes, only change the state, but do not run chef-client
        node.set_state("crowbar_upgrade")
      else
        node["crowbar_wall"]["crowbar_upgrade_step"] = "crowbar_upgrade"
        node.save
        nodes_to_upgrade.push node.name
      end
    end

    # adapt current proposal, so the nodes get crowbar-upgrade role
    proposal["deployment"]["crowbar"]["elements"]["crowbar-upgrade"] = nodes_to_upgrade
    proposal.save
    # commit the proposal so chef recipe get executed
    self.proposal_commit("default", false, false)
  end

  def revert_nodes_from_crowbar_upgrade
    proposal = ProposalObject.find_proposal('crowbar', 'default')

    NodeObject.all.each do |node|
      if node.state == "crowbar_upgrade"
        # revert nodes to previous state; mark the wall so apply does not change state again
        node["crowbar_wall"]["crowbar_upgrade_step"] = "revert_to_ready"
        node.save
        node.set_state("ready")
      end
    end

    # commit current proposal (with the crowbar-upgrade role still assigned to nodes),
    # so the recipe is executed when nodes have 'ready' state
    self.proposal_commit("default", false, false)
    # now remove the nodes from upgrade role
    proposal["deployment"]["crowbar"]["elements"]["crowbar-upgrade"] = []
    proposal.save
  end

  def apply_role_pre_chef_call(old_role, role, all_nodes)
    @logger.debug("crowbar apply_role_pre_chef_call: entering #{all_nodes.inspect}")
    all_nodes.each do |n|
      node = NodeObject.find_node_by_name n
      # value of crowbar_wall["crowbar_upgrade"] indicates that the role should be executed
      # but node state should not be changed: this is needed when reverting node state to ready
      if node.role?("crowbar-upgrade") && node.crowbar_wall["crowbar_upgrade_step"] != "revert_to_ready"
        node.set_state("crowbar_upgrade")
      end
    end
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
        @logger.fatal("Deploying proposal - barclamp: #{k}, name: #{plist[:instances].join(',')}")
        plist[:instances].each do |v|
          id = "default"
          data = {"id" => id}
          @logger.fatal("Deploying proposal - id: #{id}, name: #{v.inspect}")

          if v != "default"
            data = JSON.parse(
              File.read(v)
            )

            id = data["id"].gsub("bc-#{k}-", "")
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
              answer = service.proposal_create(data)
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
                answer = service.proposal_commit(id, false, false)
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
      order = BarclampCatalog.run_order(bc_name)
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
      options[:raid] = {} if options[:raid].nil?
      options[:bios] = {} if options[:bios].nil?

      options[:show] << :raid if options[:raid].length > 0
      options[:show] << :bios if options[:bios].length > 0
    end
    options
  end

  def self.pretty_target_platform(target_platform)
    return "SLES 12" if target_platform == "suse-12.0"
    return "SLES 11 SP3" if target_platform == "suse-11.3"
    return "Windows Server 2012 R2" if target_platform == "windows-6.3"
    return "Windows Server 2012" if target_platform == "windows-6.2"
    return "Hyper-V Server 2012 R2" if target_platform == "hyperv-6.3"
    return "Hyper-V Server 2012" if target_platform == "hyperv-6.2"
    return target_platform
  end

  def self.require_license_key?(target_platform)
    require_license_platforms.include? target_platform
  end

  def self.require_license_platforms
    [
      "windows-6.3",
      "windows-6.2"
    ]
  end

  def self.support_software_raid
    [
      "suse-12.0",
      "suse-11.3"
    ]
  end

  protected

  def transition_to_readying(inst, name, state, node = nil)
    only_unless_admin node do
      process_raid_claims node
    end
  end

  def process_raid_claims(node)
    unless node.raid_type == "single"
      node["filesystem"].each do |device, attributes|
        if device =~ /\/dev\/md\d+$/
          process_raid_device node, device, attributes
        else
          process_raid_member node, device, attributes
        end
      end

      process_raid_boot node
    end
  end

  def process_raid_device(node, device, attributes)
    if ["/", "/boot"].include? attributes["mount"]
      unique_name = node.unique_device_for(
        ::File.basename(device.to_s).to_s
      )

      return if unique_name.nil?

      unless node.disk_owner(unique_name) == "OS"
        node.disk_release unique_name, node.disk_owner(unique_name)
        node.disk_claim unique_name, "OS"
        self.transition_save_node = true
      end
    end
  end

  def process_raid_member(node, device, attributes)
    if attributes["fs_type"] == "linux_raid_member"
      unique_name = node.unique_device_for(
        ::File.basename(device.to_s).to_s.gsub(/[0-9]+$/, "")
      )

      return if unique_name.nil?

      unless node.disk_owner(unique_name) == "Raid"
        node.disk_release unique_name, node.disk_owner(unique_name)
        node.disk_claim unique_name, "Raid"
        self.transition_save_node = true
      end
    end
  end

  def process_raid_boot(node)
    boot_device = node["filesystem"].sort.map do |device, attributes|
      if ["/", "/boot"].include? attributes["mount"]
        node.unique_device_for(
          ::File.basename(device.to_s)
        )
      end
    end.compact.first

    unless boot_device == node.crowbar_wall["boot_device"]
      node.boot_device boot_device
      self.transition_save_node = true
    end
  end
end
