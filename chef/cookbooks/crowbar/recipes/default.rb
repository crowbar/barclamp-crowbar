#
# Cookbook Name:: crowbar
# Recipe:: default
#
# Copyright 2011, Opscode, Inc. and Dell, Inc
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

directory "/root/.chef" do
  owner "root"
  group "root"
  mode "0700"
  action :create
end

cookbook_file "/root/.chef/knife.rb" do
  owner "root"
  group "root"
  mode "0600"
  action :create
  source "knife.rb"
end

directory "/home/crowbar/.chef" do
  owner "crowbar"
  group "crowbar"
  mode "0700"
  action :create
end

cookbook_file "/home/crowbar/.chef/knife.rb" do
  owner "crowbar"
  group "crowbar"
  mode "0600"
  action :create
  source "knife.rb"
end

if node[:platform] == "suse"
  cookbook_file "/etc/init.d/crowbar" do
    owner "root"
    group "root"
    mode "0755"
    action :create
    source "crowbar.suse"
  end

  link "/usr/sbin/rccrowbar" do
    action :create
    to "/etc/init.d/crowbar"
    not_if "test -L /usr/sbin/rccrowbar"
  end

  bash "Enable crowbar service" do
    code "/sbin/chkconfig crowbar on"
    not_if "/sbin/chkconfig crowbar | grep -q on"
  end
end
