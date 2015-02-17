module CrowbarHelper
  def self.get_host_for_admin_url(node, use_cluster = false)
    if use_cluster && defined?(CrowbarPacemakerHelper)
      # loose dependency on the pacemaker cookbook
      cluster_vhostname = CrowbarPacemakerHelper.cluster_vhostname(node)
      "#{cluster_vhostname}.#{node[:domain]}"
    else
      node[:fqdn]
    end
  end

  def self.get_host_for_public_url(node, use_ssl, use_cluster = false)
    if use_cluster && defined?(CrowbarPacemakerHelper)
      # loose dependency on the pacemaker cookbook
      cluster_vhostname = CrowbarPacemakerHelper.cluster_vhostname(node)

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
end
