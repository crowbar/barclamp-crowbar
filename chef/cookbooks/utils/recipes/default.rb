# Copyright (c) 2011 Dell Inc.
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

# set some shortcut variables. 
@@centos = @@ubuntu = false 
platform = node[:platform]
case platform
  when "centos", "redhat"
  @@centos = true
  when "ubuntu"
  @@ubuntu = true
end

@@is_admin = node["crowbar"]["admin_node"] rescue false

log("running on OS:[#{platform}] on #{node[:dmi][:system][:product_name]} hardware #{@@is_admin ? 'admin': ''}") { level :info} 

