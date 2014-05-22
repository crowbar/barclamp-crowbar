# -*- encoding : utf-8 -*-
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

define :==BC-MODEL==_service do
  ==BC-MODEL==_name="==BC-MODEL==-#{params[:name]}"

  service ==BC-MODEL==_name do
    if (platform?("ubuntu") && node.platform_version.to_f >= 10.04)
      restart_command "restart #{==BC-MODEL==_name}"
      stop_command "stop #{==BC-MODEL==_name}"
      start_command "start #{==BC-MODEL==_name}"
      status_command "status #{==BC-MODEL==_name} | cut -d' ' -f2 | cut -d'/' -f1 | grep start"
    end

    supports :status => true, :restart => true
    action [:enable, :start]
    subscribes :restart, resources(:template => node[:==BC-MODEL==][:config_file])
  end
end

