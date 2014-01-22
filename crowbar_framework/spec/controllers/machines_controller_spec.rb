require 'spec_helper'

describe MachinesController do
  before do
    NodeObject.any_instance.stubs(:system).returns(true)
  end

  describe "GET index" do
    before do
      FileTest.stubs(:exist?).returns(true)
    end

    it "is successful" do
      get :index
      response.should be_success
    end

    it "renders json with node names" do
      get :index
      assigns(:app).should_not be_empty
      json = JSON.parse(response.body)
      json.should be_a(Array)
      json.should_not be_empty
    end
  end

  describe "GET show" do
    it "is successful" do
      get :show, :name => "testing"
      response.should be_success
    end

    it "renders the node as json hash" do
      get :show, :name => "testing.crowbar.com"
      json = JSON.parse(response.body)
      json["name"].should == "testing.crowbar.com"
    end

    it "renders 404 if node not found" do
      get :show, :name => "nonexistent"
      response.should be_missing
    end
  end

  describe "GET list" do
    it "responds with index" do
      MachinesController.any_instance.expects(:index).once
      get :list
    end
  end

  describe "POST action" do
    it "assigns @name in a before filter based on param passed" do
      post :reboot, :name => "testing"
      assigns(:name).should == "testing.#{session[:domain]}"
    end
  end

  context "Non existent node" do
    describe "POST action" do
      it "fails with JSON when w/ not found" do
        post :reinstall, :name => "nonexistent"
        response.should be_missing
        expect {
          JSON.parse(response.body)
        }.to_not raise_error
      end
    end

=begin
    FIXME: decide if this is a bug
      it "fails with a reasonable error when name not passed" do
        post :reinstall
        response.should be_bad_request
      end
    end
=end

    ["poweron", "allocate", "delete", "reboot", "shutdown", "identify", "reset", "update", "reinstall"].each do |operation|
      describe "POST #{operation}" do
        it "renders error JSON w/ not alert found" do
          response = post operation, :name => "nonexistent"
          response.should be_missing
          JSON.parse(response.body)["alert"].should == "Host not found"
        end

        it "does not call node operation" do
          NodeObject.any_instance.expects(operation).never
        end
      end
    end
  end

  context "existing node" do
    ["reinstall", "update", "reset", "delete"].each do |operation|
      describe "POST #{operation}" do
        it "sends #{operation} to the node" do
          post operation, :name => "testing"
        end

        it "responds with empty json as a response" do
          post operation, :name => "testing"
          json = JSON.parse(response.body)
          json.should == {}
        end

        it "is successful" do
          post operation, :name => "testing"
          response.should be_success
        end
      end
    end

    ["identify", "shutdown", "reboot", "poweron", "allocate"].each do |operation|
      describe "POST #{operation}" do
        it "sends #{operation} to the node" do
          post operation, :name => "testing"
        end

        it "responds with empty json as a response" do
          post operation, :name => "testing"
          json = JSON.parse(response.body)
          json.should == {}
        end

        it "is successful" do
          post operation, :name => "testing"
          response.should be_success
        end
      end
    end
  end
end
