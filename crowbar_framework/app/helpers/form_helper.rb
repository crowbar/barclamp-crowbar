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

module FormHelper
  def platforms_for_select(selected)
    options_for_select(
      available_platforms.select { |p|
        # Only allow SLES 12 for SUSE Enterprise Storage
        Crowbar::Product::is_ses? ? p == "suse-12.0" : true
      }.map { |p| [crowbar_service.pretty_target_platform(p), p] },
      selected: selected.to_s,
      disabled: disabled_platforms
    )
  end

  def roles_for_select(selected)
    options_for_select(
      [
        # These roles have to stay in sync
        # with machines role command
        [t(".no_role"), "no_role"], 
        [t(".controller"), "controller"], 
        [t(".compute"), "compute"], 
        [t(".network"), "network"], 
        [t(".storage"), "storage"]
      ],
      selected.to_s
    )
  end

  def beautify_disk_owner(owner)
    @owner2label ||= {
      "Ceph" => t(".disk_role_ceph"),
      "Cinder" => t(".disk_role_cinder"),
      "Swift" => t(".disk_role_swift"),
      "LVM_DRBD" => t(".disk_role_drbd"),
      "sbd" => t(".disk_role_sbd")
    }
    @owner2label.fetch(owner, owner)
  end

  def disk_roles_for_select(selected)
    options_for_select(
      [
        [t(".disk_role_undecided"), ""],
        [t(".disk_role_ceph"), "Ceph"],
        [t(".disk_role_cinder"), "Cinder"],
        [t(".disk_role_swift"), "Swift"],
        [t(".disk_role_drbd"), "LVM_DRBD"],
        [t(".disk_role_sbd"), "sbd"]
      ],
      selected.to_s
    )
  end

  def booleans_for_select(selected)
    options_for_select(
      [
        ["true", "true"],
        ["false", "false"]
      ],
      selected.to_s
    )
  end

  def raids_for_select(selected)
    options_for_select(
      [
        [t(".raid_types.single"), "single"],
        [t(".raid_types.raid1"), "raid1"],
        [t(".raid_types.raid5"), "raid5"],
        [t(".raid_types.raid6"), "raid6"],
        [t(".raid_types.raid10"), "raid10"]
      ],
      selected.to_s
    )
  end

  def drives_for_select(selected)
    available = @node.physical_drives.map do |name, drive|
      ["/dev/#{name} (#{number_to_human_size(drive["size"].to_i * 512)})", "/dev/#{name}"]
    end.sort

    options_for_select(
      available,
      selected
    )
  end

  def instance_selector_select(bc, name, field, proposal)
    service = eval("#{bc.camelize}Service.new nil")
    options = service.list_active[1] | service.proposals[1]

    if options.empty?
      options = [["None", ""]]
    else
      options = options.map do |x|
        [x.humanize,x]
      end
    end

    def_val = proposal.raw_attributes || ""

    field.split("/").each do |f|
      next if f.empty?
      break if def_val == ""
      def_val = def_val[f] || ""
    end

    select_tag(
      name,
      options_for_select(options, def_val),
      :class => "form-control",
      "data-change" => "#{field};#{name};string"
    )
  end

  def render_instance_selector(bc, name, label, field, proposal)
    return unless ServiceObject.get_service(bc).allow_multiple_proposals?

    render :partial => "barclamp/instance_selector", :locals => {
      :field => field,
      :label => label,
      :select_tag => instance_selector_select(bc, name, field, proposal)
    }
  end

  def number_field_tag(name, value = nil, options = {})
    options = options.stringify_keys
    options["type"] ||= "number"

    if range = options.delete("in") || options.delete("within")
      options.update("min" => range.min, "max" => range.max)
    end

    text_field_tag(name, value, options)
  end

  def defaults_for_select(options, item, scope = "")
    options_for_select(
      option_default(options, item, scope),
      item
    )
  end

  protected

  def option_default(options, item, scope = "")
    h = options.clone

    if h.nil?
      h = [
        [t(item, :scope => scope), item]
      ]
    else
      index = h.index do |x|
        x.last == item
      end

      if index.nil?
        label = if item == ChefObject::NOT_SET or item.nil? or item.empty?
          t(ChefObject::NOT_SET, :scope => scope)
        else
          item.humanize
        end

        h.push [
          wrap_around(label),
          item || ChefObject::NOT_SET
        ]
      else
        label, value = h[index]

        unless label.start_with?("[")
          h[index] = [wrap_around(label), value]
        end
      end
    end

    h
  end

  def wrap_around(value, first = "[", last = "]")
    [
      first,
      value,
      last
    ].join
  end
end
