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

require 'pp'
require 'chef'
require 'json'
require 'hash_only_merge'
require 'securerandom'
require 'timeout'

class ServiceObject
  include CrowbarPacemakerProxy

  FORBIDDEN_PROPOSAL_NAMES=["template","nodes","commit","status"]

  attr_accessor :bc_name
  attr_accessor :logger
  attr_accessor :validation_errors

  def initialize(thelogger)
    @bc_name = 'unknown'
    @logger = thelogger
    @validation_errors = []
  end

  def self.get_service(name)
    Kernel.const_get("#{name.camelize}Service")
  end

  # OVERRIDE AS NEEDED! true if barclamp can have multiple proposals
  def self.allow_multiple_proposals?
    false
  end

  # This provides the suggested name for new proposals.
  # OVERRIDE AS NEEDED!
  def self.suggested_proposal_name
    I18n.t("proposal.items.default")
  end

  def role_constraints
    self.class.role_constraints
  end

  class << self
    include CrowbarPacemakerProxy

    # This method should be overriden from subclassing service objects
    # and return the constraints related to this specific service.
    def role_constraints
      {}
    end
  end

  def validation_error message
    @logger.warn message
    @validation_errors << message
  end

  def simple_proposal_ui?
    proposals = Proposal.where(barclamp: "crowbar")

    result = false
    unless proposals[0].nil? or proposals[0]["attributes"].nil? or proposals[0]["attributes"]["crowbar"].nil?
      if not proposals[0]["attributes"]["crowbar"]["simple_proposal_ui"].nil?
        result = proposals[0]["attributes"]["crowbar"]["simple_proposal_ui"]
      end
    end
    return result
  end

  def self.barclamp_catalog
    BarclampCatalog.catalog
  end

  def self.bc_name
    self.name.underscore[/(.*)_service$/,1]
  end

  # ordered list of barclamps from groups in the crowbar.yml files.
  # Built at barclamp install time by the catalog step
  def self.members
    BarclampCatalog.members(bc_name)
  end

  def self.all
    # The catalog contains more than just barclamps - it has also barclamp
    # groups. So we filter out barclamps by attempting to create a proposal
    # (which loads the barclamps JSON metadata). Only those that pass
    # are valid barclamps.
    BarclampCatalog.barclamps.map do |name, attrs|
      Proposal.new(barclamp: name) rescue nil
    end.compact.map do |prop|
      [prop.barclamp, prop["description"]]
    end.to_h
  end

  def self.run_order(bc, cat = nil)
    BarclampCatalog.run_order(bc)
  end

  def run_order
    BarclampCatalog.run_order(@bc_name)
  end

  def self.chef_order(bc, cat = nil)
    BarclampCatalog.chef_order(bc)
  end

  def chef_order
    BarclampCatalog.chef_order(@bc_name)
  end

  # Approach copied from libraries/secure_password.rb in the openssl cookbook
  def random_password(size = 12)
    pw = String.new
    while pw.length < size
      # SecureRandom actually wraps around
      # OpenSSL::Random.random_bytes (falling back to /dev/urandom),
      # but it ensures a random seed first.
      # Note that we only accept (a subset of) ASCII characters; otherwise, we
      # get unicode characters that chef cannot store.
      pw << SecureRandom.base64(size).gsub(/[\+\/=]/, "")
    end
    pw[-size,size]
  end

#
# Helper routines for queuing
#

  def set_to_applying(nodes, inst)
    Crowbar::Lock.new(logger: @logger, path: Rails.root.join("tmp", "BA-LOCK.lock")).with_lock do
      nodes.each do |node_name|
        node = NodeObject.find_node_by_name(node_name)
        next if node.nil?

        node.crowbar["state"] = "applying"
        node.crowbar["state_owner"] = "#{@bc_name}-#{inst}"
        node.save
      end
    end
  end

  def restore_to_ready(nodes)
    Crowbar::Lock.new(logger: @logger, path: Rails.root.join("tmp", "BA-LOCK.lock")).with_lock do
      nodes.each do |node_name|
        node = NodeObject.find_node_by_name(node_name)
        next if node.nil?

        node.crowbar['state'] = 'ready'
        node.crowbar['state_owner'] = ""
        node.save
      end
    end
  end

#
# Queuing routines:
#   queue_proposal - attempts to queue proposal returns delay otherwise.
#   dequeue_proposal - remove item from queue and clean up
#   process_queue - see what we can execute
#

  def queue_proposal(inst, element_order, elements, deps, bc = @bc_name)
    Crowbar::DeploymentQueue.new(logger: @logger).queue_proposal(bc, inst, elements, element_order, deps)
  end

  def dequeue_proposal(inst, bc = @bc_name)
    Crowbar::DeploymentQueue.new(logger: @logger).dequeue_proposal(bc, inst)
  end

  def process_queue
    Crowbar::DeploymentQueue.new(logger: @logger).process_queue
  end
#
# update proposal status information
#
  # FIXME: refactor into Proposal#status=()
  def update_proposal_status(inst, status, message, bc = @bc_name)
    @logger.debug("update_proposal_status: enter #{inst} #{bc} #{status} #{message}")

    prop = Proposal.where(barclamp: bc, name: inst).first
    unless prop.nil?
      prop["deployment"][bc]["crowbar-status"] = status
      prop["deployment"][bc]["crowbar-failed"] = message
      res = prop.save
    else
      res = true
    end

    @logger.debug("update_proposal_status: exit #{inst} #{bc} #{status} #{message}")
    res
  end

#
# API Functions
#
  def versions
    [200, { :versions => [ "1.0" ] }]
  end

  def transition
    [200, {}]
  end

  def list_active
    roles = RoleObject.find_roles_by_name("#{@bc_name}-config-*")
    roles.map! { |r| r.name.gsub("#{@bc_name}-config-","") } unless roles.empty?
    [200, roles]
  end

  def show_active(inst)
    inst = "#{@bc_name}-config-#{inst}"

    role = RoleObject.find_role_by_name(inst)

    if role.nil?
      [404, "Active instance not found"]
    else
      [200, role]
    end
  end

  # FIXME: Move into proposal before_save filter
  def clean_proposal(proposal)
    @logger.debug "clean_proposal"
    proposal.delete("controller")
    proposal.delete("action")
    proposal.delete("barclamp")
    proposal.delete("name")
    proposal.delete("utf8")
    proposal.delete("_method")
    proposal.delete("authenticity_token")
  end

  def destroy_active(inst)

    role_name = "#{@bc_name}-config-#{inst}"
    @logger.debug "Trying to deactivate role #{role_name}"
    role = RoleObject.find_role_by_name(role_name)
    return [404, {}] if role.nil?
    reverse_deps = RoleObject.reverse_dependencies(role_name)
    if !reverse_deps.empty?
      raise(I18n.t('model.service.would_break_dependency', :name => @bc_name, :dependson => reverse_deps.to_sentence))
    else
      # By nulling the elements, it functions as a remove
      dep = role.override_attributes
      dep[@bc_name]["elements"] = {}
      dep[@bc_name].delete("elements_expanded")
      @logger.debug "#{inst} proposal has a crowbar-committing key" if dep[@bc_name]["config"].has_key? "crowbar-committing"
      dep[@bc_name]["config"].delete("crowbar-committing")
      dep[@bc_name]["config"].delete("crowbar-queued")
      role.override_attributes = dep
      answer = profile("Apply role #{role_name} #{inst}") { apply_role(role, inst, false) }
      role.destroy
      answer
    end
  end

  # FIXME: these methods operate on a proposal and the controller has access o
  # bc_name/inst anyway. So it might be better to not pollute the inheritance
  # chain.
  def elements
    [200, Proposal.new(barclamp: @bc_name).all_elements]
  end

  def element_info(role = nil)
    nodes = NodeObject.find_all_nodes.map(&:name)

    return [200, nodes] unless role

    valid_roles = Proposal.new(barclamp: @bc_name).all_elements
    return [400, "No role #{role} found for #{@bc_name}."] if !valid_roles.include?(role)

    # FIXME: we could try adding each node in turn to existing proposal's 'elements' and removing it
    # from the nodes list in the case the new proposal would not be valid, so
    # nodes that can't be added at all would not be returned.
    nodes.reject! do |node|
      node_is_valid_for_role(node, role.to_s)
    end

    [200, nodes]
  end

  def proposals_raw
    Proposal.where(barclamp: @bc_name)
  end

  def proposals
    props = proposals_raw
    props = props.map { |p| p["id"].gsub("bc-#{@bc_name}-", "") }
    [200, props]
  end

  def proposal_show(inst)
    prop = Proposal.where(barclamp: @bc_name, name: inst).first
    if prop.nil?
      [404, {}]
    else
      [200, prop]
    end
  end

  #
  # Utility method to find instances for barclamps we depend on
  #
  # FIXME: a registry that could be queried for active barclamps
  def find_dep_proposal(bc, optional=false)
    begin
      const_service = self.class.get_service(bc)
    rescue
      @logger.info "Barclamp \"#{bc}\" is not available."
      proposals = []
    else
      service = const_service.new @logger
      proposals = service.list_active[1]
      proposals = service.proposals[1] if proposals.empty?
    end

    if proposals.empty? || proposals[0].blank?
      if optional
        @logger.info "No optional \"#{bc}\" dependency proposal found for \"#{@bc_name}\" proposal."
      else
        raise(I18n.t('model.service.dependency_missing', :name => @bc_name, :dependson => bc))
      end
    end

    # Return empty string instead of nil, because the attributes referring to
    # proposals are generally required in the schema
    proposals[0] || ""
  end

  def node_is_valid_for_role(node, role)
    elements = { role => [node] }
    violates_admin_constraint?(elements, role) ||
      violates_platform_constraint?(elements, role) ||
      violates_exclude_platform_constraint?(elements, role) ||
      violates_cluster_constraint?(elements, role)
  end

  # Helper to select nodes that make sense on proposal creation
  def select_nodes_for_role(all_nodes, role, preferred_intended_role = nil)
    # do not modify array given by caller
    valid_nodes = all_nodes.dup

    valid_nodes.delete_if { |n| n.nil? }

    valid_nodes.reject! do |node|
      node_is_valid_for_role(node.name, role)
    end

    unless preferred_intended_role.nil?
      preferred_all_nodes = valid_nodes.select { |n| n.intended_role == preferred_intended_role }
      valid_nodes = preferred_all_nodes unless preferred_all_nodes.empty?
    end

    if role_constraints[role] && role_constraints[role].has_key?("count") && role_constraints[role]["count"] >= 0
      valid_nodes = valid_nodes.take(role_constraints[role]["count"])
    end

    valid_nodes
  end

  #
  # This can be overridden to provide a better creation proposal
  #
  # FIXME: check if it is overridden and move to caller
  def create_proposal
    prop = Proposal.new(barclamp: @bc_name)
    raise(I18n.t('model.service.template_missing', :name => @bc_name )) if prop.nil?
    prop.raw_data
  end

  # FIXME: looks like purely controller methods
  def proposal_create(params)
    base_id = params["id"]
    params["id"] = "bc-#{@bc_name}-#{params["id"]}"
    if FORBIDDEN_PROPOSAL_NAMES.any?{|n| n == base_id}
      return [403,I18n.t('model.service.illegal_name', names: FORBIDDEN_PROPOSAL_NAMES.to_sentence)]
    end

    prop = Proposal.where(barclamp: @bc_name, name: base_id).first
    return [400, I18n.t('model.service.name_exists')] unless prop.nil?
    return [400, I18n.t('model.service.too_short')] if base_id.length == 0
    return [400, I18n.t('model.service.illegal_chars')] if base_id =~ /[^A-Za-z0-9_]/

    proposal = create_proposal
    proposal["deployment"][@bc_name]["config"]["environment"] = "#{@bc_name}-config-#{base_id}"

    # crowbar-deep-merge-template key should be removed in all cases, as it
    # should not end in the proposal anyway; if the key is not here, we default
    # to false (and therefore the old behavior)
    if params.delete("crowbar-deep-merge-template")
      HashOnlyMerge.hash_only_merge!(proposal, params)
    else
      proposal.merge!(params)
    end

    clean_proposal(proposal)

    # When we create a proposal, it might be "invalid", as some roles might be missing
    # This is OK, as the next step for the user is to add nodes to the roles
    # But we need to skip the after_save validations in the _proposal_update
    _proposal_update(@bc_name, base_id, proposal, false)
  end

  def proposal_edit(params, validate_after_save = true)
    base_id = params["id"] || params[:name]
    params["id"] = "bc-#{@bc_name}-#{base_id}"
    proposal = {}.merge(params)
    clean_proposal(proposal)
    _proposal_update(@bc_name, base_id, proposal, validate_after_save)
  end

  def proposal_delete(inst)
    prop = Proposal.where(barclamp: @bc_name, name: inst).first
    if prop.nil?
      [404, {}]
    else
      prop.destroy
      [200, {}]
    end
  end

  # FIXME: most of these can be validations on the model itself,
  # preferrably refactored into Validator classes.
  def save_proposal!(prop, options = {})
    options.reverse_merge!(:validate_after_save => true)
    clean_proposal(prop.raw_data)
    validate_proposal(prop.raw_data)
    validate_proposal_elements(prop.elements)
    prop.save
    validate_proposal_after_save(prop.raw_data) if options[:validate_after_save]
  end

  # XXX: this is where proposal gets copied into a role, scheduling / ops order
  # is computed (in apply_role) and chef client gets called on the nodes.
  # Hopefully, this will get moved into a background job.
  def proposal_commit(inst, in_queue = false, validate_after_save = true)
    prop = Proposal.where(barclamp: @bc_name, name: inst).first

    if prop.nil?
      [404, "#{I18n.t('.cannot_find', :scope=>'model.service')}: #{@bc_name}.#{inst}"]
    elsif prop["deployment"][@bc_name]["crowbar-committing"]
      [402, "#{I18n.t('.already_commit', :scope=>'model.service')}: #{@bc_name}.#{inst}"]
    else
      response = nil
      begin
        # Put mark on the wall
        prop["deployment"][@bc_name]["crowbar-committing"] = true
        save_proposal!(prop, :validate_after_save => validate_after_save)
        response = active_update prop.raw_data, inst, in_queue
      rescue Chef::Exceptions::ValidationFailed => e
        @logger.error ([e.message] + e.backtrace).join("\n")
        response = [400, "Failed to validate proposal: #{e.message}"]
      rescue StandardError => e
        @logger.error ([e.message] + e.backtrace).join("\n")
        response = [500, e.message]
      ensure
        # Make sure we unmark the wall
        prop.reload
        prop["deployment"][@bc_name]["crowbar-committing"] = false
        prop.latest_applied = (response.first == 200)
        prop.save
      end
      response
    end
  end

  def display_name
    @display_name ||= BarclampCatalog.display_name(@bc_name)
  end

  #
  # This can be overridden.  Specific to node validation.
  #
  # FIXME: move into validator classes
  def validate_proposal_elements proposal_elements
    proposal_elements.each do |role_and_elements|
      role, elements = role_and_elements
      uniq_elements  = elements.uniq

      if uniq_elements.length != elements.length
        raise I18n.t('proposal.failures.duplicate_elements_in_role') + " " + role
      end

      uniq_elements.each do |element|
        if is_cluster? element
          unless cluster_exists? element
            raise I18n.t('proposal.failures.unknown_cluster') + " " + cluster_name(element)
          end
        else
          nodes = NodeObject.find_nodes_by_name element
          if nodes.nil? || nodes.empty?
            raise I18n.t('proposal.failures.unknown_node') + " " + element
          end
        end
      end
    end
  end

  def proposal_schema_directory
    Rails.root.join("..", "chef", "data_bags", "crowbar").expand_path
  end

  #
  # This can be overridden to get better validation if needed.
  #
  def validate_proposal proposal
    path = proposal_schema_directory
    begin
      validator = CrowbarValidator.new("#{path}/bc-template-#{@bc_name}.schema")
    rescue StandardError => e
      Rails.logger.error("failed to load databag schema for #{@bc_name}: #{e.message}")
      Rails.logger.debug e.backtrace.join("\n")
      raise Chef::Exceptions::ValidationFailed.new( "failed to load databag schema for #{@bc_name}: #{e.message}" )
    end
    Rails.logger.info "validating proposal #{@bc_name}"

    errors = validator.validate(proposal)
    @validation_errors = errors.map {|e| e.message}
    handle_validation_errors
  end

  #
  # This does additional validation of the proposal, but after it has been
  # saved. This should be used if the errors are easy to fix in the proposal.
  #
  # This can be overridden to get better validation if needed. Call it
  # after your overriden method for error handling and constraints validation.
  #
  def validate_proposal_after_save proposal
    validate_proposal_constraints proposal
    handle_validation_errors
  end

  def violates_count_constraint?(elements, role)
    if role_constraints[role] && role_constraints[role].has_key?("count")
      len = elements[role].length
      max_count = role_constraints[role]["count"]
      max_count >= 0 && len > max_count
    else
      false
    end
  end

  def violates_uniqueness_constraint?(elements, role)
    if role_constraints[role] && role_constraints[role]["unique"]
      elements[role].each do |element|
        elements.keys.each do |loop_role|
          next if loop_role == role
          return true if elements[loop_role].include? element
        end
      end
    end
    false
  end

  def violates_conflicts_constraint?(elements, role)
    if role_constraints[role] && role_constraints[role]["conflicts_with"]
      conflicts = role_constraints[role]["conflicts_with"].select do |conflicting_role|
        elements[role].any? do |element|
          elements[conflicting_role] && elements[conflicting_role].include?(element)
        end
      end
      return true if conflicts.count > 0
    end
    false
  end

  def violates_admin_constraint?(elements, role, nodes_is_admin = {})
    if role_constraints[role] && !role_constraints[role]["admin"]
      elements[role].each do |element|
        next if is_cluster? element
        unless nodes_is_admin.has_key? element
          node = NodeObject.find_node_by_name(element)
          nodes_is_admin[element] = (!node.nil? && node.admin?)
        end
        return true if nodes_is_admin[element]
      end
    end
    false
  end

  def violates_platform_constraint?(elements, role)
    if role_constraints[role] && role_constraints[role].has_key?("platform")
      constraints = role_constraints[role]["platform"]
      elements[role].each do |element|
        next if is_cluster? element
        node = NodeObject.find_node_by_name(element)

        return true if !constraints.any? do |platform, version|
          PlatformRequirement.new(platform, version).satisfied_by?(node[:platform], node[:platform_version])
        end
      end
    end
    false
  end

  def violates_exclude_platform_constraint?(elements, role)
    if role_constraints[role] && role_constraints[role].has_key?("exclude_platform")
      constraints = role_constraints[role]["exclude_platform"]
      elements[role].each do |element|
        next if is_cluster? element
        node = NodeObject.find_node_by_name(element)

        return true if constraints.any? do |platform, version|
          PlatformRequirement.new(platform, version).satisfied_by?(node[:platform], node[:platform_version])
        end
      end
    end
    false
  end

  def violates_cluster_constraint?(elements, role)
    if role_constraints[role] && !role_constraints[role]["cluster"]
      clusters = elements[role].select {|e| is_cluster? e}
      unless clusters.empty?
        return true
      end
    end
    false
  end

  #
  # Ensure that the proposal respects constraints defined for the roles
  #
  def validate_proposal_constraints(proposal)
    elements = proposal["deployment"][@bc_name]["elements"]
    nodes_is_admin = {}

    role_constraints.keys.each do |role|
      next unless elements.has_key?(role)

      if violates_count_constraint?(elements, role)
        validation_error("Role #{role} can accept up to #{role_constraints[role]["count"]} elements only.")
      end

      if violates_uniqueness_constraint?(elements, role)
        validation_error("Elements assigned to #{role} cannot be assigned to another role.")
        break
      end

      if violates_conflicts_constraint?(elements, role)
        validation_error("Element cannot be assigned to both role #{role} and any of these roles: #{role_constraints[role]["conflicts_with"].join(", ")}")
        break
      end

      if violates_admin_constraint?(elements, role, nodes_is_admin)
        validation_error("Role #{role} does not accept admin nodes.")
        break
      end

      if violates_platform_constraint?(elements, role)
        platforms = role_constraints[role]["platform"].map {|k, v| [k, v].join(' ')}.join(', ')
        validation_error("Role #{role} can be used only for #{platforms} platform(s).")
      end

      if violates_exclude_platform_constraint?(elements, role)
        platforms = role_constraints[role]["exclude_platform"].map {|k, v| [k, v].join(' ')}.join(', ')
        validation_error("Role #{role} can't be used for #{platforms} platform(s).")
      end

      if violates_cluster_constraint?(elements, role)
        validation_error("Role #{role} does not accept clusters.")
      end
    end
  end

  #
  # Ensure that the proposal contains exactly one node for role
  #
  def validate_one_for_role(proposal, role)
    elements = proposal["deployment"][@bc_name]["elements"]

    if not elements.has_key?(role) or elements[role].length != 1
      validation_error("Need one (and only one) #{role} node.")
    end
  end

  #
  # Ensure that the proposal contains at least n nodes for role
  #
  def validate_at_least_n_for_role(proposal, role, n)
    elements = proposal["deployment"][@bc_name]["elements"]

    if not elements.has_key?(role) or elements[role].length < n
      validation_error("Need at least #{n} #{role} node#{"s" if n > 1}.")
    end
  end

  #
  # Ensure that the proposal contains an odd number of nodes for role
  #
  def validate_count_as_odd_for_role(proposal, role)
    elements = proposal["deployment"][@bc_name]["elements"]

    if not elements.has_key?(role) or elements[role].length.to_i.even?
      validation_error("Need an odd number of #{role} nodes.")
    end
  end

  def validate_dep_proposal_is_active(bc, proposal)
    const_service = self.class.get_service(bc)
    service = const_service.new @logger
    proposals = service.list_active[1].to_a
    unless proposals.include?(proposal)
      if const_service.allow_multiple_proposals?
        validation_error("Proposal \"#{proposal}\" for #{service.display_name} is not active yet.")
      else
        validation_error("Proposal for #{service.display_name} is not active yet.")
      end
    end
  end

  def _proposal_update(bc_name, inst, proposal, validate_after_save = true)
    prop = Proposal.where(barclamp: bc_name, name: inst).first_or_initialize(barclamp: bc_name, name: inst)

    begin
      prop.properties = proposal
      save_proposal!(prop, :validate_after_save => validate_after_save)
      Rails.logger.info "saved proposal"
      [200, {}]
    rescue Net::HTTPServerException => e
      Rails.logger.error ([e.message] + e.backtrace).join("\n")
      [e.response.code, {}]
    rescue Chef::Exceptions::ValidationFailed => e2
      Rails.logger.error ([e2.message] + e2.backtrace).join("\n")
      [400, "Failed to validate proposal: #{e2.message}"]
    end
  end

  #
  # This is a role output function
  # Can take either a RoleObject or a Role.
  #
  # FIXME: check if it is ever used except for controller
  def self.role_to_proposal(role, bc_name)
    proposal = {}

    proposal["id"] = role.name.gsub("#{bc_name}-config-", "bc-#{bc_name}-")
    proposal["description"] = role.description
    proposal["attributes"] = role.default_attributes
    proposal["deployment"] = role.override_attributes

    proposal
  end

  #
  # From a proposal json
  #
  def self.proposal_to_role(proposal, bc_name)
    role = Chef::Role.new
    role.name proposal["id"].gsub("bc-#{bc_name}-", "#{bc_name}-config-")
    role.description proposal["description"]
    role.default_attributes proposal["attributes"]
    role.override_attributes proposal["deployment"]
    RoleObject.new role
  end

  #
  # After validation, this is where the role is applied to the system The old
  # instance (if one exists) is compared with the new instance.  roles are
  # removed and delete roles are added (if they exist) for nodes leaving roles
  # roles are added for nodes joining roles.  Calls chef-client on nodes
  #
  # This function can be overriden to define a barclamp specific operation.  A
  # call is provided that receives the role and all string names of the nodes
  # before the chef-client call
  #
  # The in_queue signifies if apply_role was called from deployment queue's
  # process_queue, and prevents recursion.
  def apply_role(role, inst, in_queue)
    @logger.debug "apply_role(#{role.name}, #{inst}, #{in_queue})"

    # Part I: Looking up data & checks
    #
    # we look up the role in the database (if there is one), the new one is
    # passed in as the role param.
    #
    # From both, we need 'elements', i.e. role -> nodes map and element_order
    # -> an ordered list of roles, telling us in which order they should be
    # applied.  I.e., it gives dependency info within a barclamp.
    #
    # Any of the new role's elements can contain clusters, so we need to expand
    # them to individual nodes. We store them in 'elements_expanded'.  Keeping
    # role's elements_expanded cache field fresh is handled by pacemaker
    # barclamp.
    #
    # We also check that all nodes we'll require are in the ready state.
    #

    # Initialize variables used in ensure at the end of the method
    chef_daemon_nodes = []

    # get all nodes except the admin node and windows nodes which don't have ssh
    nodes_without_admin = []
    NodeObject.find_all_nodes.each do |node|
      unless node[:platform] == "windows" || node.admin?
        nodes_without_admin << node.name
      end
    end
    apply_lock = Crowbar::Lock.new(logger: @logger, path: "/var/chef/cache/daemon-pause-file.lock", remote: nodes_without_admin).acquire

    # Query for this role
    old_role = RoleObject.find_role_by_name(role.name)

    # Get the new elements list
    new_deployment = role.override_attributes[@bc_name]
    new_elements = new_deployment["elements"]
    element_order = new_deployment["element_order"]

    #
    # Attempt to queue the proposal.  If delay is empty, then run it.
    #
    deps = proposal_dependencies(role)
    delay, pre_cached_nodes = queue_proposal(inst, element_order, new_elements, deps)
    return [202, delay] unless delay.empty?

    @logger.debug "delay empty - running proposal"

    # expand items in elements that are not nodes
    expanded_new_elements = {}
    new_deployment["elements"].each do |role_name, nodes|
      expanded_new_elements[role_name], failures = expand_nodes_for_all(nodes)
      unless failures.nil? || failures.empty?
        @logger.fatal("apply_role: Failed to expand items #{failures.inspect} for role \"#{role_name}\"")
        message = "Failed to apply the proposal: cannot expand list of nodes for role \"#{role_name}\", following items do not exist: #{failures.join(", ")}"
        update_proposal_status(inst, "failed", message)
        process_queue unless in_queue
        return [ 405, message ]
      end
    end
    new_elements = expanded_new_elements

    # save list of expanded elements, as this is needed when we look at the old
    # role. See below the comments for old_elements.
    if new_elements != new_deployment["elements"]
      new_deployment["elements_expanded"] = new_elements
    else
      new_deployment.delete("elements_expanded")
    end

    # make sure the role is saved
    role.save

    # Build a list of old elements.
    # elements_expanded on the old role is guaranteed to exists, as we already
    # ran through apply_role with the old_role.  Cache is used for the case
    # when pacemaker barclamp is deactivated.  elements_expanded gets updated
    # by pacemaker barclamp.
    old_elements = {}
    old_deployment = old_role.override_attributes[@bc_name] unless old_role.nil?
    unless old_deployment.nil?
      old_elements = old_deployment["elements_expanded"]
      if old_elements.nil?
        old_elements = old_deployment["elements"]
      end
    end
    element_order = old_deployment["element_order"] if (!old_deployment.nil? and element_order.nil?)

    @logger.debug "old_deployment #{old_deployment.pretty_inspect}"
    @logger.debug "new_deployment #{new_deployment.pretty_inspect}"

    # Part II. Creating add/remove changesets.
    #
    # For Role ordering
    runlist_priority_map = new_deployment["element_run_list_order"] || { }
    local_chef_order = chef_order()
    role_map = new_deployment["element_states"]
    role_map = {} unless role_map

    # deployment["element_order"] tells us which order the various
    # roles should be applied, and deployment["elements"] tells us
    # which nodes each role should be applied to.  We need to "join
    # the dots" between these two, to build lists of pending role
    # addition/removal actions, which will allow us to perform the
    # correct operations on the nodes' run lists, and then run
    # chef-client in the correct order.  So we build a
    # pending_node_actions Hash which maps each node name to a Hash
    # representing pending role addition/removal actions for that
    # node, e.g.
    #
    #   {
    #     :remove => [ role1_to_remove, ... ],
    #     :add    => [ role1_to_add,    ... ]
    #   }
    pending_node_actions = {}

    # List of all *new* nodes which will be changed (sans deleted ones)
    all_nodes = []

    # We'll build an Array where each item represents a batch of work,
    # and the batches must be performed sequentially in this order.
    # This will mirror the ordering specified by element_order below,
    # but the sub-arrays of run_order will be Arrays of names of the
    # involved nodes, whereas the sub-arrays of element_order are Arrays
    # of names of Chef roles.
    run_order = []

    # get proposal to remember potential removal of a role
    proposal = Proposal.where(barclamp: @bc_name, name: inst).first
    save_proposal = false

    # element_order is an Array where each item represents a batch of roles and
    # the batches must be applied sequentially in this order.
    element_order.each do |roles|
      # roles is an Array of names of Chef roles which can all be
      # applied in parallel.
      @logger.debug "elems #{roles.inspect}"

      # A list of nodes changed when applying roles from this batch
      nodes_in_batch = []

      roles.each do |role_name|
        # Ignore _remove roles in case they're listed here, as we automatically
        # handle them
        next if role_name =~ /_remove$/

        old_nodes = old_elements[role_name] || []
        new_nodes = new_elements[role_name] || []

        @logger.debug "role_name #{role_name.inspect}"
        @logger.debug "old_nodes #{old_nodes.inspect}"
        @logger.debug "new_nodes #{new_nodes.inspect}"

        remove_role_name = "#{role_name}_remove"

        # Also act on nodes that were to be removed last time, but couldn't due
        # to possibly some error on last application
        old_nodes += (proposal.elements.delete(remove_role_name) || [])

        # We already have nodes with old version of this role.
        unless old_nodes.empty?
          # Lookup remove-role.
          tmprole = RoleObject.find_role_by_name remove_role_name
          use_remove_role = !tmprole.nil?

          old_nodes.each do |node_name|
            node = NodeObject.find_node_by_name(node_name)

            # Don't add deleted nodes to the run order, they clearly won't have
            # the old role
            if node.nil?
              @logger.debug "skipping deleted node #{node_name}"
              next
            end

            pre_cached_nodes[node_name] = node

            # An old node that is not in the new deployment, drop it
            unless new_nodes.include?(node_name)
              @logger.debug "remove node #{node_name}"
              pending_node_actions[node_name] = { :remove => [], :add => [] } if pending_node_actions[node_name].nil?

              pending_node_actions[node_name][:remove] << role_name

              # Remove roles are  a way to "de-configure" things on the node
              # when a role is not used anymore for that node. For instance,
              # stopping a service, or removing packages.
              # FIXME: it's not clear how/who should be responsible for
              # removing them from the node records.
              if use_remove_role
                pending_node_actions[node_name][:add] << remove_role_name

                # Save remove intention in #{@bc_name}-databag; we will remove
                # the intention after a successful apply_role.
                proposal.elements[remove_role_name] ||= []
                proposal.elements[remove_role_name] << node_name
                save_proposal ||= true
              end

              nodes_in_batch << node_name unless nodes_in_batch.include?(node_name)
            end
          end
        end

        # If new_nodes is empty, we are just removing the proposal.
        unless new_nodes.empty?
          new_nodes.each do |node_name|
            node = NodeObject.find_node_by_name(node_name)

            # Don't add deleted nodes to the run order
            #
            # Q: Why don't we just bail out instead?
            # A: This got added for the barclamps where all nodes are used (for
            # instance, provisioner, logging, dns, ntp); so that we don't fail
            # too easily when a node got forgotten.
            # It's kind of a ugly workaround for the fact that we don't
            # properly handle forgotten node and for the fact that we don't
            # have some alias that be used to assign all existing nodes to a
            # role (which would be an improvement over the requirement to
            # explicitly list all nodes).
            if node.nil?
              @logger.debug "skipping deleted node #{node_name}"
              next
            end

            pre_cached_nodes[node_name] = node

            all_nodes << node_name unless all_nodes.include?(node_name)

            # A new node that we did not know before
            unless old_nodes.include?(node_name)
              @logger.debug "add node #{node_name}"
              pending_node_actions[node_name] = { :remove => [], :add => [] } if pending_node_actions[node_name].nil?
              pending_node_actions[node_name][:add] << role_name
            end
            nodes_in_batch << node_name unless nodes_in_batch.include?(node_name)
          end
        end
      end # roles.each

      @logger.debug "nodes_in_batch #{nodes_in_batch.inspect}"
      run_order << nodes_in_batch unless nodes_in_batch.empty?
      @logger.debug "run_order #{run_order.inspect}"
    end

    # save databag with the role removal intention
    proposal.save if save_proposal

    # mark nodes as applying; beware that all_nodes do not contain nodes that
    # are actually removed
    applying_nodes = run_order.flatten
    set_to_applying(applying_nodes, inst)

    # Part III: Update run lists of nodes to reflect new deployment. I.e. write
    # through the deployment schedule in pending node actions into run lists.
    @logger.debug "Clean the run_lists for #{pending_node_actions.inspect}"

    admin_nodes = []

    pending_node_actions.each do |node_name, lists|
      # pre_cached_nodes contains only new_nodes, we need to look up the
      # old ones as well.
      node = pre_cached_nodes[node_name]
      if node.nil?
        node = NodeObject.find_node_by_name(node_name)
        pre_cached_nodes[node_name] = node
      end
      next if node.nil?

      admin_nodes << node_name if node.admin?

      save_it = false

      rlist = lists[:remove]
      alist = lists[:add]

      @logger.debug "rlist #{rlist.pretty_inspect}"
      @logger.debug "alist #{alist.pretty_inspect}"

      # Remove the roles being lost
      rlist.each do |item|
        next unless node.role? item
        @logger.debug("AR: Removing role #{item} to #{node.name}")
        node.delete_from_run_list item
        save_it = true
      end

      # Add the roles being gained
      alist.each do |item|
        next if node.role? item
        priority = runlist_priority_map[item] || local_chef_order
        @logger.debug("AR: Adding role #{item} to #{node.name} with priority #{priority}")
        node.add_to_run_list(item, priority, role_map[item])
        save_it = true
      end

      # Make sure the config role is on the nodes in this barclamp, otherwise
      # remove it
      # FIXME: All nodes is basically all new nodes, which should have an
      # add/remove list, why do we need to add specifically the passed role?
      if all_nodes.include?(node.name)
        # Add the config role
        unless node.role?(role.name)
          priority = runlist_priority_map[role.name] || local_chef_order
          @logger.debug("AR: Adding role #{role.name} to #{node.name} with priority #{priority}")
          node.add_to_run_list(role.name, priority, role_map[role.name])
          save_it = true
        end
      else
        # Remove the config role
        if node.role?(role.name)
          @logger.debug("AR: Removing role #{role.name} to #{node.name}")
          node.delete_from_run_list role.name
          save_it = true
        end
      end

      @logger.debug("AR: Saving node #{node.name}") if save_it
      node.save if save_it
    end

    # Part IV: Deployment. Running chef clients as separate processes, each
    # independent batch is parallelized, admin and non-admin nodes are treated
    # separately. The whole deploy runs twice (possibly to cover up transient
    # errors). Lastly, chef client is executed manually on this (admin) node,
    # to make sure admin node changes are deployed.

    # Deployment pre (and later post) callbacks.
    # The barclamps override these.
    begin
      apply_role_pre_chef_call(old_role, role, all_nodes)
    rescue StandardError => e
      @logger.fatal("apply_role: Exception #{e.message} #{e.backtrace.join("\n")}")
      message = "Failed to apply the proposal: exception before calling chef (#{e.message})"
      update_proposal_status(inst, "failed", message)
      restore_to_ready(applying_nodes)
      process_queue unless in_queue
      return [ 405, message ]
    end

    # Each batch is a list of nodes that can be done in parallel.
    ran_admin = false
    run_order.each do |batch|
      next if batch.empty?
      @logger.debug "batch #{batch.inspect}"

      non_admin_nodes = []
      admin_list = []
      batch.each do |node_name|
        # Run admin nodes a different way.
        if admin_nodes.include?(node_name)
          @logger.debug "#{node_name} is in admin_nodes #{admin_nodes.inspect}"
          admin_list << node_name
          ran_admin = true
          next
        end
        non_admin_nodes << node_name
      end

      @logger.debug("AR: Calling chef-client for #{role.name} on non-admin nodes #{non_admin_nodes.join(" ")}")
      @logger.debug("AR: Calling chef-client for #{role.name} on admin nodes #{admin_list.join(" ")}")

      #
      # XXX: We used to do this twice - do we really need twice???
      # Yes! We do!  The system has some transient issues that are hidden
      # but the double run for failing nodes.  For now, we will do this.
      # Make this better one day.
      #
      pids = {}
      unless non_admin_nodes.empty?
        non_admin_nodes.each do |node|
          nobj = pre_cached_nodes[node] || NodeObject.find_node_by_name(node)
          unless nobj[:platform] == "windows"
            filename = "#{ENV['CROWBAR_LOG_DIR']}/chef-client/#{node}.log"
            pid = run_remote_chef_client(node, "chef-client", filename)
            pids[pid] = node
          end
        end
        status = Process.waitall
        badones = status.select { |x| x[1].exitstatus != 0 }

        unless badones.empty?
          unless oneshot?
            badones.each do |baddie|
              node = pids[baddie[0]]
              @logger.warn("Re-running chef-client again for a failure: #{node} #{@bc_name} #{inst}")
              filename = "#{ENV['CROWBAR_LOG_DIR']}/chef-client/#{node}.log"
              pid = run_remote_chef_client(node, "chef-client", filename)
              pids[pid] = node
            end
            status = Process.waitall
            badones = status.select { |x| x[1].exitstatus != 0 }
          end

          unless badones.empty?
            message = "Failed to apply the proposal to: "
            badones.each do |baddie|
              message = message + "#{pids[baddie[0]]} \n"+ get_log_lines("#{pids[baddie[0]]}")
            end
            update_proposal_status(inst, "failed", message)
            restore_to_ready(applying_nodes)
            process_queue unless in_queue
            return [ 405, message ]
          end
        end
      end

      unless admin_list.empty?
        admin_list.each do |node|
          filename = "#{ENV['CROWBAR_LOG_DIR']}/chef-client/#{node}.log"
          pid = run_remote_chef_client(node, Rails.root.join("..", "bin", "single_chef_client.sh").expand_path.to_s, filename)
          pids[node] = pid
        end
        status = Process.waitall
        badones = status.select { |x| x[1].exitstatus != 0 }

        unless badones.empty?
          unless oneshot?
            badones.each do |baddie|
              node = pids[baddie[0]]
              @logger.warn("Re-running chef-client (admin) again for a failure: #{node} #{@bc_name} #{inst}")
              filename = "#{ENV['CROWBAR_LOG_DIR']}/chef-client/#{node}.log"
              pid = run_remote_chef_client(node, Rails.root.join("..", "bin", "single_chef_client.sh").expand_path.to_s, filename)
              pids[pid] = node
            end
            status = Process.waitall
            badones = status.select { |x| x[1].exitstatus != 0 }
          end

          unless badones.empty?
            message = "Failed to apply the proposal to: "
            badones.each do |baddie|
              message = message + "#{pids[baddie[0]]} \n "+ get_log_lines("#{pids[baddie[0]]}")
            end
            update_proposal_status(inst, "failed", message)
            restore_to_ready(applying_nodes)
            process_queue unless in_queue
            return [ 405, message ]
          end
        end
      end
    end

    # XXX: This should not be done this way.  Something else should request this.
    system("sudo", "-i", Rails.root.join("..", "bin", "single_chef_client.sh").expand_path.to_s) if !ran_admin

    # are there any roles to remove from the runlist?
    # The @bcname proposal's elements key will contain the removal intentions
    # proposal.elements =>
    # {
    #   "role1_remove" => ["node1"],
    #   "role2_remove" => ["node2", "node3"]
    # }
    roles_to_remove = proposal.elements.keys.select do |r|
      r =~ /_remove$/
    end
    roles_to_remove.each do |role_to_remove|
      # No need to remember the nodes with the role to remove, now that we've
      # executed the role, hence the delete()
      nodes_with_role_to_remove = proposal.elements.delete(role_to_remove)
      nodes_with_role_to_remove.each do |node_name|
        node = pre_cached_nodes[node_name]
        node.delete_from_run_list(role_to_remove)
        node.save
      end
    end

    # Save if we did a change
    proposal.save unless roles_to_remove.empty?

    # Post deploy callback
    begin
      apply_role_post_chef_call(old_role, role, all_nodes)
    rescue StandardError => e
      @logger.fatal("apply_role: Exception #{e.message} #{e.backtrace.join("\n")}")
      message = "Failed to apply the proposal: exception after calling chef (#{e.message})"
      update_proposal_status(inst, "failed", message)
      restore_to_ready(applying_nodes)
      process_queue unless in_queue
      return [ 405, message ]
    end

    update_proposal_status(inst, "success", "")
    restore_to_ready(applying_nodes)
    process_queue unless in_queue
    [200, {}]
  ensure
    apply_lock.release
  end

  def apply_role_pre_chef_call(old_role, role, all_nodes)
    # noop by default.
  end

  def apply_role_post_chef_call(old_role, role, all_nodes)
    # noop by default.
  end

  def oneshot?
    # by default, allow running again in case of chef failures
    return false
  end

  #
  # Inputs: role = RoleObject of proposal being applied/queued.
  # Returns: List of hashs { "barclamp" => bcname, "inst" => instname }
  #
  def proposal_dependencies(role)
    # Default none
    []
  end

  def add_role_to_instance_and_node(barclamp, instance, name, prop, role, newrole)
    node = NodeObject.find_node_by_name name
    if node.nil?
      @logger.debug("ARTOI: couldn't find node #{name}. bailing")
      return false
    end

    runlist_priority_map = prop["deployment"][barclamp]["element_run_list_order"] rescue {}
    runlist_priority_map ||= {}
    role_map = prop["deployment"][barclamp]["element_states"] rescue {}
    role_map = {} unless role_map

    local_chef_order = runlist_priority_map[newrole] || BarclampCatalog.chef_order(barclamp)

    prop["deployment"][barclamp]["elements"][newrole] = [] if prop["deployment"][barclamp]["elements"][newrole].nil?
    unless prop["deployment"][barclamp]["elements"][newrole].include?(node.name)
      @logger.debug("ARTOI: updating proposal with node #{node.name}, role #{newrole} for deployment of #{barclamp}")
      prop["deployment"][barclamp]["elements"][newrole] << node.name
      prop.save
    else
      @logger.debug("ARTOI: node #{node.name} already in proposal: role #{newrole} for #{barclamp}")
    end

    role.override_attributes[barclamp]["elements"][newrole] = [] if role.override_attributes[barclamp]["elements"][newrole].nil?
    unless role.override_attributes[barclamp]["elements"][newrole].include?(node.name)
      @logger.debug("ARTOI: updating role #{role.name} for node #{node.name} for barclamp: #{barclamp}/#{newrole}")
      role.override_attributes[barclamp]["elements"][newrole] << node.name
      role.save
    else
      @logger.debug("ARTOI: role #{role.name} already has node #{node.name} for barclamp: #{barclamp}/#{newrole}")
    end

    save_it = false
    unless node.role?(newrole)
      node.add_to_run_list(newrole, local_chef_order, role_map[newrole])
      save_it = true
    end

    unless node.role?("#{barclamp}-config-#{instance}")
      node.add_to_run_list("#{barclamp}-config-#{instance}", local_chef_order)
      save_it = true
    end

    if save_it
      @logger.debug("saving node")
      node.save
    end
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

      ssh = ["sudo", "-u", "root", "--", "ssh", "-o", "TCPKeepAlive=no", "-o", "ServerAliveInterval=15", "root@#{node}"]
      ssh_cmd = ssh.dup << command

      # check if there are currently other chef-client runs on the node
      wait_for_chef_clients(node, :logger => false)
      # check if the node is currently rebooting
      wait_for_reboot(node)

      exit(1) unless system(*ssh_cmd)

      # check if we need to wait for a node reboot
      wait_for_reboot(node)
    }
  end

  private

  def wait_for_chef_clients(node_name, options = {})
    options = if options.fetch(:logger)
      {:logger => @logger}
    else
      {}
    end
    @logger.debug("wait_for_chef_clients: Waiting for already running chef-clients on #{node_name}.")
    unless RemoteNode.chef_ready?(node_name, 1200, 10, options)
      @logger.error("Waiting for already running chef-clients on #{node_name} failed.")
      exit(1)
    end
  end

  def wait_for_reboot(node)
    nobj = NodeObject.find_node_by_name(node)
    if nobj[:crowbar_wall][:wait_for_reboot]
      puts "Waiting for reboot of node #{node}"
      if RemoteNode.ready?(node, 1200)
        puts "Waiting for reboot of node #{node} done. Node is back"
        # Check node state - crowbar_join's chef-client run should successfully finish
        puts "Waiting to finish chef-client run on node #{node}"
        begin
          Timeout.timeout(600) do
            loop do
              nobj = NodeObject.find_node_by_name(node)
              case nobj[:state]
              when "ready"
                puts "Node state after reboot is: #{nobj[:state]}. Continue"
                break
              when "problem"
                STDERR.puts "Node state after reboot is: #{nobj[:state]}. Exit"
                exit(1)
              else
                puts "Node state after reboot is: #{nobj[:state]}. Waiting"
                sleep(10)
              end
            end
          end
        rescue Timeout::Error
          STDERR.puts "Node state never reached valid state. Exit"
          exit(1)
        end
      else
        STDERR.puts "Waiting for reboot of node #{node} failed"
        exit(1)
      end
    end
  end

  def handle_validation_errors
    if @validation_errors && @validation_errors.length > 0
      Rails.logger.info "validation errors in proposal #{@bc_name}"
      raise Chef::Exceptions::ValidationFailed.new("#{@validation_errors.join("\n")}\n")
    end
  end

  def get_log_lines(pid)
    begin
      l_counter = 1
      find_counter = 0
      f = File.open("/var/log/crowbar/chef-client/#{pid}.log")
      f.each do |line|
        if line == "="*80
           find_counter = l_counter
        end
        l_counter += 1
      end
      f.seek(0, IO::SEEK_SET)
      if (find_counter > 0) && (l_counter - find_counter) < 50
        "Most recent logged lines from the Chef run: \n\n" + f.readlines[find_counter -3..l_counter].join(" ")
      else
        "Most recent logged lines from the Chef run: \n\n" + f.readlines[l_counter-50..l_counter].join(" ")
      end
    rescue
      @logger.error("Error reporting: Couldn't open /var/log/crowbar/chef-client/#{pid}.log ")
      raise "Error reporting: Couldn't open  /var/log/crowbar/chef-client/#{pid}.log"
    end
  end

  #
  # Proposal is a json structure (not a ProposalObject)
  # Use to create or update an active instance
  #
  def active_update(proposal, inst, in_queue)
    begin
      role = ServiceObject.proposal_to_role(proposal, @bc_name)
      profile("Apply role #{role.name} #{inst}") { apply_role(role, inst, in_queue) }
    rescue Net::HTTPServerException => e
      Rails.logger.error ([e.message] + e.backtrace).join("\n")
      [e.response.code, {}]
    rescue Chef::Exceptions::ValidationFailed => e2
      Rails.logger.error ([e2.message] + e2.backtrace).join("\n")
      [400, e2.message]
    end
  end

  def only_if_admin(node)
    yield if node.admin?
  end

  def only_unless_admin(node)
    yield unless node.admin?
  end

  def profile(name, &block)
    if ENV["ENABLE_PROFILER"] == "true"
      Rack::MiniProfiler.step(name, &block)
    else
      block.call
    end
  end
end
