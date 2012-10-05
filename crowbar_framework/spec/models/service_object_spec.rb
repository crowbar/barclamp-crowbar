require 'spec_helper'

describe ServiceObject do
  
  before(:each) do
    @service_object = ServiceObject.new(nil)
  end
  
  it "should have a logger" do
    @service_object.logger.should be Rails.logger
  end
  
  it "should have my logger" do
    so = ServiceObject.new(true)
    so.logger.should be true
  end
  
  it "should have a bc_name of unknown and barclamp is nil" do
    @service_object.bc_name.should eq("unknown")
    @service_object.barclamp.should be nil
  end
  
  it "should allow bc_name to be set and update barclamp" do
    @service_object.bc_name = "crowbar"
    @service_object.bc_name.should eq("crowbar")
    @service_object.barclamp.should be Barclamp.find_by_name("crowbar")
  end
  
  describe "should have a password generator" do
    it "should default to twelve character passwords" do
      string = @service_object.random_password
      string.length.should be 12
      string.should_not match /i1loO0/
    end
    
    it "should allow other lengths passwords" do
      string = @service_object.random_password 15
      string.length.should be 15
      string.should_not match /i1loO0/
    end
  end



end
