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

require "spec_helper"

describe SupportController do
  describe "GET index" do
    it "is successful" do
      get :index
      response.should be_success
    end
  end

  describe "GET export_chef" do
    it "displays flash message on error" do
      NodeObject.stubs(:all).raises(StandardError)
      get :export_chef
      response.should redirect_to(utils_url)
      flash[:alert].should_not be_empty
    end

    it "exports known data into db dir" do
      begin
        now = Time.now
        Time.stubs(:now).returns(now)

        Process.stubs(:fork).returns(0)

        filename = "crowbar-chef-#{now.strftime("%Y%m%d-%H%M%S")}.tgz"
        export = Rails.root.join("db", filename)

        get :export_chef
        flash[:alert].should be_nil
        response.should redirect_to(utils_url(:waiting => true, :file => filename))

        Dir.glob(Rails.root.join("db", "*.json")).count.should_not == 0
      ensure
        Dir.glob(Rails.root.join("db", "*.json")).each { |json| FileUtils.rm(json) }
      end
    end
  end
end

