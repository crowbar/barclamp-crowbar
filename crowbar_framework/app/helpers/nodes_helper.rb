# Copyright 2011-2013, Dell
# Copyright 2013, SUSE LINUX Products GmbH
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
# Author: Rob Hirschfeld
# Author: SUSE LINUX Products GmbH
#

module NodesHelper
  def piechart_for(group)
    values = [
      group[:status]["ready"],
      group[:status]["failed"],
      group[:status]["unknown"],
      group[:status]["unready"] + group[:status]["pending"]
    ]

    if group[:status]["building"]
      values << values.pop + group[:status]["building"]
    end

    tooltip = [].tap do |result|
      result.push content_tag(
        :strong,
        t(".status_pie.total", :count => values.sum)
      )

      result.push t(".status_pie.ready", :count => values[0]) if values[0] > 0
      result.push t(".status_pie.unknown", :count => values[1]) if values[1] > 0
      result.push t(".status_pie.unready", :count => values[2]) if values[2] > 0
      result.push t(".status_pie.pending", :count => values[3]) if values[3] > 0
    end

    content_tag(
      :span,
      "",
      :title => tooltip.join(tag(:br)),
      "data-piechart" => values.join(","),
      "data-tooltip" => "true"
    )
  end

  def format_memory(kbyte)
    sprintf(
      "%#1.2f GB",
      kbyte.to_f / 1024 / 1024
    )
  end

  def node_list_for(value)
    {}.tap do |nodes|
      nodes_hash.each do |k, v|
        nodes[k] = v if value.member? k
      end
    end.sort_by { |k, v| v[:alias] }
  end

  def node_links_for(value)
    node_list_for(value).map do |name, node|
      link_to node[:alias], nodes_path(:selected => node[:handle]), :title => node[:title]
    end
  end

  def nodes_hash(group = nil)
    @nodes_hash ||= {}

    @nodes_hash[group] ||= begin
      {}.tap do |nodes|
        NodeObject.all.each do |node|
          nodes[node.name] = {
            :handle => node.handle,
            :alias => node.alias,
            :title => node.description(false, true),
            :admin => node.admin?,
            :group => node.group
          } if node.group == group or group.nil?
        end
      end.sort_by{ |k, v| v[:alias] }
    end
  end

  def node_detail_attributes(node)
    raid_data = if crowbar_options[:show].include?(:raid)
      content_tag(
        :em,
        t(node.raid_set, :scope => "raid")
      )
    else
      nil
    end

    [].tap do |result|
      result.push [
        t("model.attributes.node.name"),
        dash_or(node.name)
      ]

      result.push [
        t("model.attributes.node.hardware"),
        dash_or(node.hardware)
      ]

      result.push [
        t("model.attributes.node.public_name"),
        dash_or(node.public_name)
      ]

      result.push [
        t("model.attributes.node.asset_tag"),
        dash_or(node.asset_tag)
      ]

      result.push [
        t("model.attributes.node.description"),
        dash_or(node.description)
      ]

      result.push [
        t("model.attributes.node.cpu"),
        dash_or(node.cpu)
      ]

      result.push [
        t("model.attributes.node.target_platform"),
        dash_or(node.pretty_target_platform)
      ]

      result.push [
        t("model.attributes.node.memory"),
        format_memory(node.memory)
      ]

      result.push [
        t("model.attributes.node.state"),
        t(node.state, :scope => :state, :default => node.state.titlecase)
      ]

      result.push [
        t("model.attributes.node.number_of_drives"),
        [
          node.pretty_drives,
          raid_data
        ].compact.join(" - ")
      ]

      result.push [
        t("model.attributes.node.uptime"),
        value_for(node.uptime, t("model.attributes.node.na"), node.ready?)
      ]

      result.push [
        t("model.attributes.node.mac"),
        value_for(node.mac, t("unknown"))
      ]

      result.push [
        t("model.attributes.node.allocated"),
        value_for(t(".active"), t(".inactive"), node.allocated)
      ]

      if node.switch_unit.nil?
        switch_label = [
          value_for(node.switch_name, t("unknown"), node.switch_name >= 0),
          value_for(node.switch_port, t("unknown"), node.switch_port >= 0)
        ].join(" / ")

        result.push [
          t("model.attributes.node.switch_name_port"),
          link_to(switch_label, switch_path(:node => node.handle))
        ]
      else
        switch_label = [
          value_for(node.switch_name, t("unknown"), node.switch_name >= 0),
          value_for(node.switch_unit, t("unknown"), node.switch_unit >= 0),
          value_for(node.switch_port, t("unknown"), node.switch_port >= 0)
        ].join(" / ")

        result.push [
          t("model.attributes.node.switch_name_unit_port"),
          link_to(switch_label, switch_path(:node => node.handle))
        ]
      end

      if CrowbarService.require_license_key? node.target_platform
        result.push [
          t("model.attributes.node.license_key"),
          value_for(node.license_key, t("model.attributes.node.license_key_not_set"))
        ]
      end
    end.in_groups_of(2, "")
  end

  def node_ip_list(ips)
    entries = [].tap do |result|
      ips.each do |network, addresses|
        unless network == "~notconnected" and addresses.nil?
          network_list = if network == "[not managed]"
            address_list = addresses.to_a.map do |address|
              content_tag(
                :li,
                address
              )
            end

            [
              content_tag(
                :strong,
                network
              ),
              content_tag(
                :ul,
                address_list.join("\n")
              )
            ].join("\n")
          else
            address_list = if addresses.is_a? String
              addresses
            else
              content_tag(
                :ul,
                addresses.keys.collect { |key|
                  content_tag(
                    :li,
                    "#{key}: #{addresses[key]}"
                  )
                }.join("\n")
              )
            end

            [
              content_tag(
                :strong,
                network
              ),
              address_list
            ].join("\n")
          end

          result.push content_tag(
            :li,
            network_list
          )
        end
      end
    end

    if entries.empty?
      entries.push content_tag(
        :li,
        t(".no_entry"),
        :class => "empty"
      )
    end

    content_tag(
      :ul,
      entries.join("\n")
    )
  end

  def node_barclamp_list(roles)
    return "" if roles.nil? or roles.empty?

    required_roles = roles.delete_if do |role|
      not role =~ /^.*-config-/
    end

    map_role_list(
      required_roles,
      proc { |role| role.gsub("-config-", " ").titleize },
      :class => "barclamps"
    )
  end

  def node_role_list(roles)
    return "" if roles.nil? or roles.empty?

    required_roles = roles.delete_if do |role|
      role =~ /^.*-config-/
    end

    map_role_list(
      required_roles,
      proc { |role| role },
      :class => "roles"
    )
  end

  def node_link_list(node)
    link_list = [].tap do |link|
      if node.bmc_set?
        path = node["crowbar_wall"]["ipmi"]["address"] rescue "none"

        link.push content_tag(
          :li,
          link_to(
            t(".bmc"),
            "https://#{path}"
          )
        )
      end

      unless node["crowbar"]["links"].nil?
        node["crowbar"]["links"].sort_by { |name, link| name }.each do |name, link|
          link.push content_tag(
            :li,
            link_to(
              name,
              link,
              :target => "_blank"
            )
          )
        end
      end
    end

    if link_list.empty?
      link_list.push content_tag(
        :li,
        t(".no_entry"),
        :class => "empty"
      )
    end

    content_tag(
      :ul,
      link_list.join("\n")
    )
  end

  def dl_item(term, definition, options={})
    unless definition.blank? && options[:show_if_blank] != true
      html  = "<dt>#{options[:escape_html] != false ? (h term) : (term)}</dt>"
      dd = "<dd" + (options[:class].nil? ? "" : " class=""+options[:class]+""") + (options[:title].nil? ? "" : " title="" + options[:title]+""") + ">"
      html += "#{dd}#{options[:escape_html] != false ? (h definition) : (definition)}</dd>"
      raw html
    end
  end

  protected

  def map_role_list(roles, proc, options = {})
    list_items = roles.map do |role|
      content_tag(
        :li,
        link_to(
          proc.call(role),
          "#",
          "data-href" => nodes_path(:role => role, :names_only => true, :format => "json")
        )
      )
    end

    if list_items.empty?
      list_items.push content_tag(
        :li,
        t(".no_entry"),
        :class => "empty"
      )
    end

    content_tag(
      :ul,
      list_items,
      options
    )
  end
end
