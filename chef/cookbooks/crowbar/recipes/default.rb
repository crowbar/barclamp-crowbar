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

if node[:platform] != "suse"
  include_recipe "bluepill"
end

pkglist=()
rainbows_path=""
case node[:platform]
when "ubuntu","debian"
  pkglist=%w{curl sqlite libsqlite3-dev libshadow-ruby1.8 markdown}
  rainbows_path="/var/lib/gems/1.8/bin/"
when "redhat","centos"
  pkglist=%w{curl sqlite sqlite-devel python-markdown}
  rainbows_path=""
when "suse"
  pkglist=%w{curl rubygem-rake rubygem-json rubygem-syslogger
      rubygem-sass rubygem-simple-navigation rubygem-i18n rubygem-haml
      rubygem-net-http-digest_auth rubygem-rails-2_3 rubygem-rainbows 
      rubygem-ruby-shadow }
end

pkglist.each {|p|
  package p do
    action :install
  end
}

if node[:platform] != "suse"
  gemlist=%w{rake json syslogger sass simple-navigation 
     i18n haml net-http-digest_auth rails rainbows }

  gemlist.each {|g|
    gem_package g do
      action :install
    end
  }
end

group "crowbar"

user "crowbar" do
  comment "Crowbar User"
  gid "crowbar"
  home "/home/crowbar"
  password "$6$afAL.34B$T2WR6zycEe2q3DktVtbH2orOroblhR6uCdo5n3jxLsm47PBm9lwygTbv3AjcmGDnvlh0y83u2yprET8g9/mve."
  shell "/bin/bash"
  supports :manage_home=>true
  not_if "egrep -qi '^crowbar:' /etc/passwd"
end

directory "/root/.chef" do
  owner "root"
  group "root"
  mode "0700"
  action :create
end

cookbook_file "/etc/profile.d/crowbar.sh" do
  owner "root"
  group "root"
  mode "0755"
  action :create
  source "crowbar.sh"
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

bash "Add crowbar chef client" do
  environment({'EDITOR' => '/bin/true', 'HOME' => '/root'})
  code "knife client create crowbar -a --file /opt/dell/crowbar_framework/config/client.pem -u chef-webui -k /etc/chef/webui.pem "
  not_if "export HOME=/root;knife client list -u crowbar -k /opt/dell/crowbar_framework/config/client.pem"
end

file "/opt/dell/crowbar_framework/log/production.log" do
  owner "crowbar"
  group "crowbar"
  mode "0666"
  action :create
end

file "/opt/dell/crowbar_framework/tmp/queue.lock" do
  owner "crowbar"
  group "crowbar"
  mode "0644"
  action :create
end
file "/opt/dell/crowbar_framework/tmp/ip.lock" do
  owner "crowbar"
  group "crowbar"
  mode "0644"
  action :create
end

if node[:platform] != "suse"
  file "/var/run/crowbar-webserver.pid" do
    owner "crowbar"
    group "crowbar"
    mode "0644"
    action :create
  end
else
  directory "/var/run/crowbar" do
    owner "crowbar"
    group "crowbar"
    mode "0700"
    action :create
  end
end

unless node["crowbar"].nil? or node["crowbar"]["users"].nil? or node["crowbar"]["realm"].nil?
  web_port = node["crowbar"]["web_port"]
  realm = node["crowbar"]["realm"]
  users = node["crowbar"]["users"]
  # Fix passwords into digests.
  users.each do |k,h|
    h["digest"] = Digest::MD5.hexdigest("#{k}:#{realm}:#{h["password"]}") if h["digest"].nil?
  end

  template "/opt/dell/crowbar_framework/htdigest" do
    source "htdigest.erb"
    variables(:users => users, :realm => realm)
    owner "crowbar"
    owner "crowbar"
    mode "0644"
  end
else
  web_port = 3000
  realm = nil
end

bash "set permissions" do
  code "chown -R crowbar:crowbar /opt/dell/crowbar_framework"
  not_if "ls -al /opt/dell/crowbar_framework/README | grep -q crowbar"
end

cookbook_file "/opt/dell/crowbar_framework/config.ru" do
  source "config.ru"
  owner "crowbar"
  group "crowbar"
  mode "0644"
end

template "/opt/dell/crowbar_framework/rainbows.cfg" do
  source "rainbows.cfg.erb"
  owner "crowbar"
  group "crowbar"
  mode "0644"
  variables(:web_host => "0.0.0.0", 
            :web_port => node["crowbar"]["web_port"] || 3000,
            :user => "crowbar",
            :concurrency_model => "EventMachine",
            :group => "crowbar",
            :logfile => "/opt/dell/crowbar_framework/log/production.log",
            :app_location => "/opt/dell/crowbar_framework")
end

template "/opt/dell/crowbar_framework/rainbows-dev.cfg" do
  source "rainbows.cfg.erb"
  owner "crowbar"
  group "crowbar"
  mode "0644"
  variables(:web_host => "0.0.0.0", 
            :web_port => node["crowbar"]["web_port"] || 3000,
            :user => "crowbar",
            :concurrency_model => "EventMachine",
            :group => "crowbar",
            :logfile => "/opt/dell/crowbar_framework/log/development.log",
            :app_location => "/opt/dell/crowbar_framework")
end

if node[:platform] != "suse"
  %w(chef-server-api chef-server-webui chef-solr rabbitmq-server).each do |f|
    file "/etc/logrotate.d/#{f}" do
      action :delete
    end
  end

  cookbook_file "/etc/logrotate.d/chef-server"

  template "/etc/bluepill/crowbar-webserver.pill" do
    source "crowbar-webserver.pill.erb"
  end

  bluepill_service "crowbar-webserver" do
    action [:load, :start]
  end

  cookbook_file "/etc/init.d/crowbar" do
    owner "root"
    group "root"
    mode "0755"
    action :create
    source "crowbar"
  end

  ["3", "5", "2"].each do |i|
    link "/etc/rc#{i}.d/S99xcrowbar" do
      action :create
      to "/etc/init.d/crowbar"
      not_if "test -L /etc/rc#{i}.d/S99xcrowbar"
    end
  end
else
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
