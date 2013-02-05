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
case node[:platform]
when "ubuntu","debian"
  pkglist=%w{curl sqlite sqlite3 libsqlite3-dev libshadow-ruby1.8 markdown vim}
when "redhat","centos"
  pkglist=%w{curl sqlite sqlite-devel python-markdown vim gcc-c++}
when "suse"
  pkglist=%w{curl rubygem-rake rubygem-json rubygem-syslogger
      rubygem-sass rubygem-simple-navigation rubygem-i18n rubygem-haml
      rubygem-net-http-digest_auth rubygem-rails-2_3 rubygem-puma 
      rubygem-ruby-shadow  vim}
end

pkglist.each {|p|
  package p do
    action :install
  end
}

if node[:platform] != "suse"

  # For some reaason gem_package doesn't work on Redhat.
  %w{tilt puma bundler}.each { |gem|
    bash "Install #{gem} gem" do
      code "gem install #{gem}"
    end
  }

  bash "Install gems through bundler" do
    code "cd /opt/dell/crowbar_framework ; bundle"
    not_if "test -e /opt/dell/crowbar_framework/Gemfile.lock"
  end
end

bash "Compile the Asssets" do
  code "cd /opt/dell/crowbar_framework ; RAILS_ENV=production rake assets:precompile"
  not_if "test -e /opt/dell/crowbar_framework/chef_install.done"
end

bash "Run the database migrations" do
  code "cd /opt/dell/crowbar_framework ; RAILS_ENV=production rake railties:install:migrations ; RAILS_ENV=production rake db:migrate"
  not_if "test -e /opt/dell/crowbar_framework/chef_install.done"
end

bash "Add the delayed_job components" do
  code "cd /opt/dell/crowbar_framework ; RAILS_ENV=production script/rails generate delayed_job:active_record"
  not_if "test -e /opt/dell/crowbar_framework/chef_install.done"
end

bash "Run the database migrations after delay" do
  code "cd /opt/dell/crowbar_framework ; RAILS_ENV=production rake db:migrate"
  not_if "test -e /opt/dell/crowbar_framework/chef_install.done"
end

bash "touch chef_install.done" do
  code "touch /opt/dell/crowbar_framework/chef_install.done"
  not_if "test -e /opt/dell/crowbar_framework/chef_install.done"
end

group "crowbar"

user "crowbar" do
  comment "Crowbar User"
  gid "crowbar"
  home "/home/crowbar"
  password "$6$afAL.34B$T2WR6zycEe2q3DktVtbH2orOroblhR6uCdo5n3jxLsm47PBm9lwygTbv3AjcmGDnvlh0y83u2yprET8g9/mve."
  shell "/bin/bash"
  supports  :manage_home=>true
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

directory "/var/run/crowbar" do
  owner "crowbar"
  group "crowbar"
  mode "0700"
  action :create
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

template "/opt/dell/crowbar_framework/puma.cfg" do
  source "puma.cfg.erb"
  owner "crowbar"
  group "crowbar"
  mode "0644"
  variables(:web_host => "0.0.0.0", 
            :web_port => node["crowbar"]["web_port"] || 3000,
            :environment => "production")
end

template "/opt/dell/crowbar_framework/puma-dev.cfg" do
  source "puma.cfg.erb"
  owner "crowbar"
  group "crowbar"
  mode "0644"
  variables(:web_host => "0.0.0.0", 
            :web_port => node["crowbar"]["web_port"] || 3000,
            :environment => "development")
end

if node[:platform] != "suse"
  bluepill_service "crowbar-webserver" do
    variables(:processes => [ {
                                "name" => "puma",
                                "start_command" => "puma -C puma.cfg",
                                "stdout" => "/dev/null",
                                "stderr" => "/dev/null",
                                "working_dir" => "/opt/dell/crowbar_framework",
                                "uid" => "crowbar",
                                "gid" => "crowbar",
                                "daemonize" => true
                              } ] )
    action [:create, :load]
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
