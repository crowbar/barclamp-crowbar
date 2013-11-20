action :enable do
  require 'rexml/document'

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

  # create config
  document = REXML::Document.new.add_element("uwsgi")
  config = document.add_element("uwsgi")

  # add global options merged with default
  options.each do |key, value|
    element = config.add_element(key.to_s)
    if value != true
      element.add_text(value.to_s)
    end
  end

  # add all instances of application
  instances.each_with_index do |instance, index|
    instance.each do |key, value|
      element = config.add_element(key.to_s, { "id" => index })
      if value != true
        element.add_text(value.to_s)
      end
    end
  end

  file_available, file_enable = config_files(new_resource.name)

  # Create service for application
  template "/etc/init.d/#{new_resource.service_name}" do
    source "uwsgi-service.sh.erb"
    mode "0755"
    cookbook "uwsgi"
    variables({
       :application => new_resource.name,
       :service => new_resource.service_name,
       :config => file_enable,
       :options => options
     })
    notifies :restart, "service[#{new_resource.service_name}]", :immediately
  end

  service "#{new_resource.service_name}" do
    supports :restart => true, :start => true, :stop => true, :status => true
    action [:enable, :nothing]
  end

  # write config to file
  template file_available do
    variables ({
        :config => config,
        :application => new_resource.name
    })
    cookbook "uwsgi"
    source "uwsgi-config.xml.erb"
    owner "root"
    group "root"
    mode 00600
    notifies :restart, "service[#{new_resource.service_name}]", :immediately
  end

  # enabling config
  link file_enable do
    to file_available
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
