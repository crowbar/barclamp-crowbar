# -*- encoding : utf-8 -*-
#
# Cookbook Name:: rails
# Recipe:: default
#
# Copyright 2009, Opscode, Inc.
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

include_recipe "ruby"

case node[:platform]
when "ubuntu","debian"
  package "rails" do
    action :install
  end
else
  %w{ rails actionmailer actionpack activerecord activesupport activeresource }.each do |rails_gem|
    gem_package rails_gem do
      if node[:rails][:version]
        version node[:rails][:version]
        action :install
      else
        action :install
      end
    end
  end
end
