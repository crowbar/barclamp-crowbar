require 'spec_helper'

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

