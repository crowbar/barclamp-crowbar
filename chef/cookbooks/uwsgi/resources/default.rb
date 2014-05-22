# -*- encoding : utf-8 -*-
actions :enable, :disable

default_action :enable

attribute :name, :kind_of => String

attribute :instances, :kind_of => [Hash, Array]
attribute :options,  :kind_of => Hash
attribute :service_name, :regex => /^[a-zA-Z0-9_-]+$/

def initialize(name, run_context = nil)
  super
  set_platform_default_providers
  @action = :enable
end

private
def set_platform_default_providers
  [:ubuntu, :debian].each do |platform|
    Chef::Platform.set(
      :platform => platform,
      :resource => :uwsgi,
      :provider => Chef::Provider::Uwsgi
    )
  end
end


