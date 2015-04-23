# Copyright (c) 2014 SUSE Linux GmbH.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

module CrowbarHelper
  def self.get_host_for_admin_url(node, use_cluster = false)
    if use_cluster && defined?(CrowbarPacemakerHelper)
      # loose dependency on the pacemaker cookbook
      cluster_vhostname = CrowbarPacemakerHelper.cluster_vhostname(node)

      admin_name = CrowbarPacemakerHelper.cluster_haproxy_vadmin_name(node)
      admin_fqdn = "#{cluster_vhostname}.#{node[:domain]}"
    else
      admin_name = node[:crowbar][:admin_name]
      admin_fqdn = node[:fqdn]
    end

    if admin_name.nil? || admin_name.empty?
      admin_fqdn
    else
      admin_name
    end
  end

  def self.get_host_for_public_url(node, use_ssl, use_cluster = false)
    if use_cluster && defined?(CrowbarPacemakerHelper)
      # loose dependency on the pacemaker cookbook
      cluster_vhostname = CrowbarPacemakerHelper.cluster_vhostname(node)

      # Specify default as the configured public name
      public_name = CrowbarPacemakerHelper.cluster_haproxy_vpublic_name(node)
      public_fqdn = "public.#{cluster_vhostname}.#{node[:domain]}"
      public_net_db = Chef::DataBagItem.load('crowbar', 'public_network').raw_data
      public_ip = public_net_db["allocated_by_name"]["#{cluster_vhostname}.#{node[:domain]}"]["address"]
    else
      public_name = node[:crowbar][:public_name]
      public_fqdn = 'public.'+node[:fqdn]
      public_ip = Chef::Recipe::Barclamp::Inventory.get_network_by_type(node, "public").address
    end

    # For the public endpoint, we prefer the public name. If not set, then we
    # use the IP address except for SSL, where we always prefer a hostname
    # (for certificate validation).
    if public_name.nil? || public_name.empty?
      if use_ssl
        public_name = public_fqdn
      else
        public_name = public_ip
      end
    end

    public_name
  end

  def self.is_admin?(node)
    # XXX Node::Attribute.fetch doesn't work with a 'false' value in
    # chef10 so we have to rescue false (see e.g. CHEF-3736)
    !!node["crowbar"].fetch("admin_node", false) rescue false
  end

  def self.in_sledgehammer?(node)
    states = [ "ready", "readying", "recovering", "applying" ]
    not states.include?(node[:state])
  end
end
