#
# Cookbook Name:: apache2
# Definition:: apache_conf
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

if node.platform == "suse"
  mod_conf = "#{node[:apache][:dir]}/conf.d/#{application_name}.conf"
else
  mod_conf = "#{node[:apache][:dir]}/mods-available/#{application_name}.conf"
end

define :apache_conf do
  template mod_conf do
    source "mods/#{params[:name]}.conf.erb"
    notifies :reload, resources(:service => "apache2")
    mode 0644
  end
end
