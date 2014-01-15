require 'spec_helper'

describe MachinesController do
  before do
    ChefObject.stubs(:cloud_domain).returns("localdomain")
    RoleObject.stubs(:find_role_by_name).returns(RoleObject.new(Chef::Role.new))
  end

  let(:chef_node) { n = Chef::Node.new; n.name "testing.example.com"; n.set["license_key"] = ""; n.set["public_name"] = ""; n }
  let(:node) { NodeObject.new(chef_node) }

  describe "GET index" do
    before do
      NodeObject.stubs(:find_all_nodes).returns([node])
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
      json.first.should == node.name
    end
  end

  describe "GET show" do
    it "is successful" do
      NodeObject.stubs(:find_node_by_name).returns(node)

      get :show, :name => "testing"
      response.should be_success
    end

    it "renders the node as json hash" do
      NodeObject.stubs(:find_node_by_name).returns(node)

      get :show, :name => "testing"
      json = JSON.parse(response.body)
      json.should == node.to_hash
    end

    it "renders 404 if node not found" do
      NodeObject.stubs(:find_node_by_name).returns(nil)
      get :show, :name => "testing"
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
      NodeObject.stubs(:find_node_by_name).returns(nil)
      post :reboot, :name => "testing"
      assigns(:name).should == "testing.#{session[:domain]}"
    end
  end

  context "Non existent node" do
    before do
      NodeObject.stubs(:find_node_by_name).returns(nil)
    end

    describe "POST action" do
      it "fails with JSON when w/ not found" do
        post :reinstall, :name => "testing"
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
          response = post operation, :name => "testing"
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
    before do
      NodeObject.stubs(:find_node_by_name).returns(node)
    end

    ["reinstall", "update", "reset", "delete"].each do |operation|
      describe "POST #{operation}" do
        before do
          node.expects(:set_state).with(operation).once
        end

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
        before do
          node.expects(operation.to_sym).once
        end

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
