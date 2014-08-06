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

module SchemaMigration
  require "chef"

  def self.run
    BarclampCatalog.barclamps.each do |bc_name, details|
      run_for_bc bc_name
    end
  end

  def self.run_for_bc bc_name
    template = ProposalObject.find_proposal("template", bc_name)

    return if template.nil?
    return if template["deployment"].nil?
    return if template["deployment"][bc_name].nil?

    all_scripts = find_scripts_for_bc(bc_name)
    return if all_scripts.empty?

    begin
      validator = CrowbarValidator.new("#{data_bags_dir}/bc-template-#{bc_name}.schema")
    rescue StandardError => e
      raise "Failed to load databag schema for #{bc_name}: #{e.message}"
    end

    props = ProposalObject.find_proposals bc_name

    props.each do |prop|
      migrate_proposal(bc_name, validator, template, all_scripts, prop)

      # Attempt to do migration for the matching committed proposal
      # Note: we don't want to commit the proposal we just migrated, because it
      # might have other uncommitted changes that are not wanted.

      role_name = prop['id'].gsub("bc-#{bc_name}-", "#{bc_name}-config-")
      role = RoleObject.find_role_by_name(role_name)

      unless role.nil?
        migrate_role(bc_name, template, all_scripts, role)
      end
    end
  end

  private

  def self.data_bags_dir
    Rails.root.join("..", "chef", "data_bags", "crowbar").expand_path
  end

  def self.get_migrate_dir(bc_name)
    return File.join(data_bags_dir, 'migrate', bc_name)
  end

  def self.find_scripts_for_bc(bc_name)
    all_scripts = []

    migrate_dir = get_migrate_dir(bc_name)
    return all_scripts unless File.directory?(migrate_dir)

    Dir.entries(migrate_dir).sort.each do |file|
      next if ['.', '..'].include?(file)
      next if File.directory?(File.join(migrate_dir, file))
      all_scripts << file
    end

    return all_scripts
  end

  def self.find_scripts_for_migration(bc_name, all_scripts, old_revision, new_revision)
    scripts = []
    migrate_dir = get_migrate_dir(bc_name)

    return scripts if old_revision == new_revision

    is_upgrade = old_revision < new_revision
    if is_upgrade
      start_revision = old_revision + 1
      end_revision = new_revision
    else
      # downgrade function to reach revision X is in migration script X+1
      start_revision = new_revision + 1
      end_revision = old_revision
    end

    Range.new(start_revision, end_revision).each do |rev|
      all_scripts.each do |script|
        next unless script =~ /^0*#{rev}_.*\.rb$/
        scripts << File.join(migrate_dir, script)
      end
    end

    scripts.reverse! unless is_upgrade

    return scripts
  end

  def self.run_script(script, is_upgrade, template_attributes, template_deployment, attributes, deployment)
    # redefine upgrade/downgrade to no-op before each load
    def upgrade ta, td, a, d
      return a, d
    end
    def downgrade ta, td, a, d
      return a, d
    end

    load script

    if is_upgrade
      attributes, deployment = upgrade(template_attributes, template_deployment, attributes, deployment)
    else
      attributes, deployment = downgrade(template_attributes, template_deployment, attributes, deployment)
    end
  end

  def self.migrate_object(bc_name, template, all_scripts, attributes, deployment)
    attributes ||= Mash.new
    deployment ||= Mash.new

    current_schema_revision = deployment['schema-revision']
    if current_schema_revision.nil?
      current_schema_revision = 0
    end

    schema_revision = template['deployment'][bc_name]['schema-revision']
    if schema_revision.nil?
      schema_revision = 0
    end

    return [nil, nil] if current_schema_revision == schema_revision

    is_upgrade = current_schema_revision < schema_revision
    scripts = self.find_scripts_for_migration(bc_name, all_scripts, current_schema_revision, schema_revision)
    return [nil, nil] if scripts.empty?

    scripts.each do |script|
      # we only pass attributes and deployment to not encourage direct access
      # to the proposal
      attributes, deployment = run_script(script, is_upgrade, template['attributes'][bc_name], template['deployment'][bc_name], attributes, deployment)
    end

    deployment['schema-revision'] = schema_revision

    return attributes, deployment
  end

  def self.migrate_proposal(bc_name, validator, template, all_scripts, proposal)
    attributes = proposal['attributes'][bc_name]
    deployment = proposal['deployment'][bc_name]

    (attributes, deployment) = migrate_object(bc_name, template, all_scripts, attributes, deployment)

    return if attributes.nil? || deployment.nil?

    proposal['attributes'][bc_name] = attributes
    proposal['deployment'][bc_name] = deployment

    errors = validator.validate(proposal.raw_data)
    unless errors.empty?
      error_lines = errors.map {|e| e.message}
      error_lines.unshift "Failed to validate migrated proposal #{proposal.name} for #{bc_name}:"
      raise error_lines.join("\n")
    end

    proposal.save
  end

  def self.migrate_role(bc_name, template, all_scripts, role)
    attributes = role.default_attributes[bc_name]
    deployment = role.override_attributes[bc_name]

    (attributes, deployment) = migrate_object(bc_name, template, all_scripts, attributes, deployment)

    return if attributes.nil? || deployment.nil?

    role.default_attributes[bc_name] = attributes
    role.override_attributes[bc_name] = deployment
    role.save
  end
end
