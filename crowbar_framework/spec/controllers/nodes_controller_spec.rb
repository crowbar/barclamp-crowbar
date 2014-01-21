require 'spec_helper'

describe NodesController do
  integrate_views

  describe "GET index" do
    it "is successful" do
      get :index
      response.should be_success
    end

    it "filters out roles" do
      get :index, :role => "i dont exist"
      assigns(:nodes).should == []
    end

    it "returns specified roles" do
      get :index, :role => "crowbar-testing_crowbar_com"
      assigns(:nodes).should_not == []
    end

    it "is successful as json" do
      get :index, :format => "json"
      response.should be_success
    end

    it "sets a flash notice if no nodes found" do
      NodeObject.stubs(:all).returns([])
      get :index
      flash[:notice].should_not be_empty
    end
  end

  describe "GET list" do
    it "is successful" do
      get :list
      response.should be_success
    end
  end

  describe "GET families" do
    it "is successful" do
      get :families
      response.should be_success
    end

    it "sets populates @families with node descriptions" do
      get :families
      assigns(:families).should == {}
    end
  end

  describe "GET status" do
    it "is successful" do
      get :status
      response.should be_success
    end

    it "renders error if fetch fails" do
      NodeObject.stubs(:all).raises(Errno::ECONNREFUSED)
      get :status
      json = JSON.parse(response.body)
      json["count"].should == -2
      json["error"].should_not be_empty
    end
  end

  describe "GET show" do
    it "is successful" do
      get :show, :id => "testing"
      response.should be_success
    end

    it "fails for nonexistent node" do
      get :show, :id => "nonexistent"
      response.should be_missing
    end

    it "renders json" do
      get :show, :id => "testing", :format => "json"
      response.should be_success
    end
  end

  describe "POST hit" do
    it "sets the machine state" do
      ["reinstall", "reset", "update", "delete"].each do |action|
        NodeObject.any_instance.expects(:set_state).with(action).once
        post :hit, :req => action, :id => "testing"
      end

      ["reboot", "shutdown", "poweron", "identify", "allocate"].each do |action|
        NodeObject.any_instance.expects(action.to_sym).once
        post :hit, :req => action, :id => "testing"
      end
    end
  end
end
