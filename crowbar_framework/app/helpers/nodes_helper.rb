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
  def nodes_by_role(role)
    NodeObject.find("roles:#{role}").sort_by(&:alias)
  end

  def piechart_for(group)
    values = piechart_values(group)
    tooltip = piechart_tooltip(values)

    content_tag(
      :span,
      "",
      :title => tooltip,
      "data-piechart" => values.join(","),
      "data-tooltip" => "true"
    )
  end

  def piechart_tooltip(values)
    [].tap do |result|
      result.push content_tag(
        :strong,
        t("total", :count => values.sum, :scope => "nodes.index.status_pie")
      )

      result.push t("ready", :count => values[0], :scope => "nodes.index.status_pie") if values[0] > 0
      result.push t("unknown", :count => values[1], :scope => "nodes.index.status_pie") if values[1] > 0
      result.push t("unready", :count => values[2], :scope => "nodes.index.status_pie") if values[2] > 0
      result.push t("pending", :count => values[3], :scope => "nodes.index.status_pie") if values[3] > 0
    end.join(tag(:br))
  end

  def piechart_values(group)
    [].tap do |result|
      result.push group[:status]["ready"]
      result.push group[:status]["failed"]
      result.push group[:status]["unknown"]

      if group[:status]["building"]
        result.push group[:status]["unready"] + group[:status]["pending"] + group[:status]["building"]
      else
        result.push group[:status]["unready"] + group[:status]["pending"]
      end
    end
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
        t("model.attributes.node.uptime"),
        value_for(node.uptime, t("model.attributes.node.na"), node.ready?)
      ]

      result.push [
        t("model.attributes.node.number_of_drives"),
        [
          node.pretty_drives,
          raid_data
        ].compact.join(" - ")
      ]

      result.push [
        t("model.attributes.node.allocated"),
        value_for(t(".active"), t(".inactive"), node.allocated)
      ]

      result.push [
        t("model.attributes.node.mac"),
        value_for(node.mac, t("unknown"))
      ]

      result.push [
        t("model.attributes.node.state"),
        content_tag(
          :span,
          t(node.state, :scope => :state, :default => node.state.titlecase),
          "data-node-state" => node.handle
        )
      ]

      show_name = if node.switch_name.nil?
        false
      else
        node.switch_name >= 0
      end

      show_port = if node.switch_port.nil?
        false
      else
        node.switch_port >= 0
      end

      show_unit = if node.switch_unit.nil?
        false
      else
        node.switch_unit >= 0
      end

      if node.switch_unit.nil?
        switch_label = [
          value_for(node.switch_name, t("unknown"), show_name),
          value_for(node.switch_port, t("unknown"), show_port)
        ].join(" / ")

        switch_title = t("model.attributes.node.switch_name_port")
      else
        switch_label = [
          value_for(node.switch_name, t("unknown"), show_name),
          value_for(node.switch_unit, t("unknown"), show_unit),
          value_for(node.switch_port, t("unknown"), show_port)
        ].join(" / ")

        switch_title = t("model.attributes.node.switch_name_unit_port")
      end

      result.push [
        switch_title,
        link_to(switch_label, switch_path(:node => node.handle))
      ]

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
          network_list = if ["[not managed]", "[dhcp]"].include? network
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

  def node_wall_list(node)
    [].tap do |result|
      intended_roles_text = {
        "no_role"    => t("nodes.form.no_role"),
        "controller" => t("nodes.form.controller"),
        "compute"    => t("nodes.form.compute"),
        "network"    => t("nodes.form.network"),
        "storage"    => t("nodes.form.storage")
      }

      intended_role = node.intended_role
      intended_role = "no_role" if intended_role.blank?

      result.push [
        t("model.attributes.node.intended_role"),
        intended_roles_text[intended_role] || intended_role
      ]

      if have_openstack
        result.push [
          t("model.attributes.node.availability_zone"),
          dash_or(node.availability_zone)
        ]
      end
    end
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
    link_list = [].tap do |result|
      if node.bmc_set?
        path = node["crowbar_wall"]["ipmi"]["address"] rescue "none"

        result.push content_tag(
          :li,
          link_to(
            t(".bmc"),
            "https://#{path}"
          )
        )
      end

      unless node["crowbar"]["links"].nil?
        node["crowbar"]["links"].sort_by { |name, link| name }.each do |name, link|
          result.push content_tag(
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
