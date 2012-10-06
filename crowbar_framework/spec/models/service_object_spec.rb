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

  describe "Locking functions" do
    it "should acquire should throw an exception if file create fails" do
      File.stub(:new) {nil}
      lambda {@service_object.acquire_lock("fred")}.should raise_error(IOError, "File not available: tmp/fred.lock")
    end

    it "acquire should sleep if lock is not available" do
      f1 = mock(File)
      f1.should_receive(:flock).exactly(2).and_return(false, true)
      File.stub(:new) {f1}
      @service_object.stub!(:sleep)
      @service_object.should_receive(:sleep).exactly(1).times
      f = @service_object.acquire_lock("fred")
      f.should be f1
    end

    it "acquire should not sleep if lock is available" do
      f1 = mock(File)
      f1.should_receive(:flock).exactly(1).and_return(true)
      File.stub(:new) {f1}
      @service_object.stub!(:sleep)
      @service_object.should_receive(:sleep).exactly(0).times
      f = @service_object.acquire_lock("fred")
      f.should be f1
    end

    it "release should throw exception on nil file" do
      lambda {@service_object.release_lock(nil)}.should raise_error(IOError, "Invalid file")
    end

    it "release should call unlock and close" do
      f1 = mock(File)
      f1.should_receive(:flock).exactly(1).and_return(true)
      f1.should_receive(:close).exactly(1).and_return(true)
      @service_object.release_lock(f1)
    end

  end

  describe "API functions" do
    it "should respond to transition and default to success" do
      answer = @service_object.transition(nil, nil, nil)
      answer.should be_an_instance_of Array
      answer[0].should be 200
      answer[1].should eq("")
    end

  end

end

