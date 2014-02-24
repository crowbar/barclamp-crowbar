# -*- encoding : utf-8 -*-
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
