#
# Cookbook Name:: crowbar
# Recipe:: crowbar-upgrade
#
# Copyright 2013-2015, SUSE LINUX Products GmbH
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

# Check for node state: if it is not 'crowbar-upgrading', we're returning
# from previous upgrade state and need to re-enable chef-client
# otherwise do the disabling stuff bellow:

if node["crowbar_wall"]["crowbar_upgrade"]

  # Disable openstack services
  # We don't know which openstack services are enabled on the node and
  # collecting that information via the attributes provided by chef is
  # rather complicated. So instead we fall back to a simple bash hack

  bash "disable_openstack_services" do
    code <<-EOF
      for i in /etc/init.d/openstack-* /etc/init.d/openvswitch-switch /etc/init.d/ovs-usurp-config-* /etc/init.d/drbd /etc/init.d/openais;
      do
        if test -e $i
        then
          initscript=`basename $i`
          chkconfig -d $initscript
        fi
      done
    EOF
    only_if { node[:platform] == "suse" }
  end

  # Disable crowbar-join
  service "crowbar_join" do
    # do not stop it, it would change node's state
    action :disable
    only_if { node[:platform] == "suse" }
  end

  # Disable chef-client
  service "chef-client" do
    action [:disable, :stop]
    only_if { node[:platform] == "suse" }
  end

else

  service "crowbar_join" do
    action :enable
    only_if { node[:platform] == "suse" }
  end

  service "chef-client" do
    action [:enable, :start]
    only_if { node[:platform] == "suse" }
  end
end
