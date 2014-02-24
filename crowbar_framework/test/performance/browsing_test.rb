# -*- encoding : utf-8 -*-
require "test_helper"
require "performance_test_help"

class BrowsingTest < ActionController::PerformanceTest
  def test_homepage
    get "/"
  end
end
