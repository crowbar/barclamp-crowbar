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

ENV["RAILS_ENV"] ||= "test"
require "simplecov"

require File.expand_path("../../config/environment", __FILE__)
require "rspec/rails"
require "rspec/autorun"
require "webmock/rspec"

Dir[Rails.root.join("spec/support/**/*.rb")].each do |f| 
  require f
end

ActiveRecord::Migration.check_pending! if defined?(ActiveRecord::Migration)
WebMock.disable_net_connect!(:allow_localhost => false)

RSpec.configure do |config|
  config.mock_with :mocha

  config.fixture_path = Rails.root.join("spec", "fixtures").to_s

  config.use_transactional_fixtures = true
  config.infer_base_class_for_anonymous_controllers = false

  config.order = "random"

  config.append_before(:each) do
    stub_request(:any, /localhost:4000/).to_rack(OfflineChef)
  end
end
