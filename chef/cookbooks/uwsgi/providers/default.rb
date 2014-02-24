# -*- encoding : utf-8 -*-
action :enable do

  # install pip and developer headers
  package "python-pip"
  package "python-dev"

  provisioner = search(:node, "roles:provisioner-server").first
  proxy_addr = provisioner[:fqdn]
  proxy_port = provisioner[:provisioner][:web_port]

  execute "pip install --index-url http://#{proxy_addr}:#{proxy_port}/files/pip_cache/simple/ uwsgi" do
    not_if "pip freeze 2>&1 | grep -i uwsgi"
  end

  # create directories for config of applications
  ["/etc/uwsgi/apps-enable", "/etc/uwsgi/apps-available"].each do |dir|
    directory dir do
      recursive true
    end
  end

  options = {
      :master => true,
      :processes => 3,
      :threads => 10,
      :protocol => :http,
      :callable => :application,
      :module => :application,
      :"buffer-size" => 12288,
      :log => ::File.join("/var/log/",new_resource.name,new_resource.name+".log")
  }
  options.merge!(new_resource.options)

  options[:pidfile] = ::File.join("/var/run/",new_resource.service_name+".pid")

  instances = new_resource.instances.is_a?(Hash) ? [new_resource.instances] : new_resource.instances

  file_available, file_enable = config_files(new_resource.name)

  # Create service for application
  template "/etc/init/#{new_resource.service_name}.conf" do
    source "uwsgi-upstart.conf.erb"
    mode "0755"
    cookbook "uwsgi"
    variables({
      :config => file_enable,
    })
  end

  link "/etc/init.d/#{new_resource.service_name}" do
    to "/lib/init/upstart-job"
  end

  # write config to file
  template file_available do
    variables ({
      :options => options,
      :instances => instances,
      :application => new_resource.name
    })
    cookbook "uwsgi"
    source "uwsgi-config.xml.erb"
    owner "root"
    group "root"
    mode 00600
  end

  # enabling config
  link file_enable do
    to file_available
  end

  service "#{new_resource.service_name}" do
    supports :restart => true, :start => true, :stop => true, :status => true
    action [:enable, :start]
    subscribes :restart, "template[/etc/init.d/#{new_resource.service_name}]", :immediately
    subscribes :restart, "template[#{file_available}]", :immediately
    subscribes :restart, "link[#{file_enable}]", :immediately
  end

end

action :disable do
  service "#{new_resource.service_name}" do
    action :stop
  end
  file config do
    action :delete
    only_if ::File.exists? config
  end
  file "/etc/init.d/#{new_resource.service_name}" do
    action :delete
    only_if ::File.exists? "/etc/init.d/#{new_resource.service_name}"
  end
end

private

def config_files application
  [::File.join("/etc/uwsgi/apps-available/",application+".xml"), ::File.join("/etc/uwsgi/apps-enable/",application+".xml")]
end
