# -*- encoding : utf-8 -*-
ENV["RAILS_ENV"] ||= "test"
require "simplecov"

require File.expand_path("../../config/environment", __FILE__)
require "rails/test_help"

class ActiveSupport::TestCase
  ActiveRecord::Migration.check_pending!
end
