#
# Cookbook Name:: apache2
# Recipe:: ssl 
#
# Copyright 2008-2009, Opscode, Inc.
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

if platform?("centos", "redhat", "fedora")
  package "mod_ssl" do
    action :upgrade
    notifies :run, resources(:execute => "generate-module-list"), :immediately
  end

  file "#{node[:apache][:dir]}/conf.d/ssl.conf" do
    action :delete
    backup false 
  end
end

# TODO: Maybe use 'line' provider's 'add_and_filter' somehow?
if File.exist?("/etc/sysconfig/apache2")
  file = File.open("/etc/sysconfig/apache2", "r")
  lines, modified = file.readlines, false
  file.close
  lines.each do |line|
    if line.start_with?('APACHE_SERVER_FLAGS') && line.scan('SSL') == []
      line.gsub!(/\"(.*)\"/, '"\\1 SSL"')
      modified = true
    end
  end
  if modified
    file = File.open("/etc/sysconfig/apache2", "w")
    lines.each {|line| file.write(line)}
    file.close
  end
end

ports = node[:apache][:listen_ports].include?("443") ? node[:apache][:listen_ports] : [node[:apache][:listen_ports], "443"].flatten

template "#{node[:apache][:dir]}/ports.conf" do
  source "ports.conf.erb"
  variables :apache_listen_ports => ports
  notifies :reload, resources(:service => "apache2")
  mode 0644
end

apache_module "ssl" do
  conf true
end
