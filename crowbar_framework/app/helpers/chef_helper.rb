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

module ChefHelper
  def chef_server_role_link(role_name)
    if ENV["CONVERGED_ADMIN"] and ENV["HAVE_CHEF_WEBUI"]
      link_to role_name, chef_server_role_url(role_name), :target => "_blank"
    else
      role_name
    end
  end

  def chef_server_role_url(role_name)
    "http://#{request.host}:4040/roles/#{role_name}"
  end
end
