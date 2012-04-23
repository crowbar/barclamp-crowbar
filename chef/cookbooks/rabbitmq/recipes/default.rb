#
# Cookbook Name:: rabbitmq
# Recipe:: default
#
# Copyright 2009, Benjamin Black
# Copyright 2009-2011, Opscode, Inc.
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

## use the RabbitMQ repository instead of Ubuntu or Debian's
## because there are very useful features in the newer versions
#apt_repository "rabbitmq" do
#  uri "http://www.rabbitmq.com/debian/"
#  distribution "testing"
#  components ["main"]
#  key "http://www.rabbitmq.com/rabbitmq-signing-key-public.asc"
#  action :add
#end

# rabbitmq-server is not well-behaved as far as managed services goes
# we'll need to add a LWRP for calling rabbitmqctl stop
# while still using /etc/init.d/rabbitmq-server start
# because of this we just put the rabbitmq-env.conf in place and let it rip

package "rabbitmq-server"

directory "/etc/rabbitmq/" do
  owner "root"
  group "root"
  mode 0755
  action :create
end

template "/etc/rabbitmq/rabbitmq-env.conf" do
  source "rabbitmq-env.conf.erb"
  owner "root"
  group "root"
  mode 0644
end  

template "/etc/rabbitmq/rabbitmq.config" do
  source "rabbitmq.config.erb"
  owner "root"
  group "root"
  mode 0644
end

user "rabbitmq" do
  action :create
  home "/var/lib/rabbitmq"
end

%w{ /var/log/rabbitmq /var/lib/rabbitmq }.each { |dir|
  directory dir do
    action :create
    owner "rabbitmq"
    group "rabbitmq"
  end
}

if node.platform == "suse"
  service "rabbitmq-server" do
    supports :status => true, :restart => true
    action [:enable, :start]
  end
else
  bash "Enable rabbit management" do
    code <<-'EOH'
  /usr/lib/rabbitmq/bin/rabbitmq-plugins enable rabbitmq_management
  /etc/init.d/rabbitmq-server restart
  exit 0
  EOH
    not_if "su - rabbitmq -s /bin/bash -c \"/usr/lib/rabbitmq/bin/rabbitmq-plugins list -E\" | grep -q rabbitmq_management"
  end
end
