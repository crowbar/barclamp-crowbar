require File.expand_path(File.join(File.dirname(__FILE__), "..", "spec_helper"))

describe MachinesController do
  before do
    RoleObject.stubs(:find_role_by_name).returns(RoleObject.new(Chef::Role.new))
  end

  let(:chef_node) { n = Chef::Node.new; n.name "testing.example.com"; n.set["license_key"] = ""; n.set["public_name"] = ""; n }
  let(:node) { NodeObject.new(chef_node) }

  describe "GET index" do
    before do
      NodeObject.stubs(:find_all_nodes).returns([node])
      FileTest.stubs(:exist?).returns(true)
      ChefObject.stubs(:cloud_domain).returns("localdomain")
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
    before do
      ChefObject.stubs(:cloud_domain).returns("localdomain")
    end

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

  context "Non existent node" do
    before do
      NodeObject.stubs(:find_node_by_name).returns(nil)
    end

=begin
    FIXME: decide if this is a bug
    describe "POST action" do
      it "fails with JSON when w/ not found" do
        post :reinstall, :name => "testing"
        response.should be_missing
        expect {
          JSON.parse(response.body)
        }.to_not raise_error
      end

      it "fails with a reasonable error when name not passed" do
        post :reinstall
        response.should be_bad_request
      end
    end
=end

    ["poweron", "allocate", "delete", "reboot", "shutdown", "identify", "reset", "update", "reinstall"].each do |operation|
      describe "POST #{operation}" do
        it "renders error text w/ not found" do
          post operation, :name => "testing"
          response.should be_missing
          response.body.should == "Host not found"
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
          get operation, :name => "testing"
        end

        it "responds with empty json as a response" do
          get operation, :name => "testing"
          json = JSON.parse(response.body)
          json.should == {}
        end

        it "is successful" do
          get operation, :name => "testing"
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
          get operation, :name => "testing"
        end

        it "responds with empty json as a response" do
          get operation, :name => "testing"
          json = JSON.parse(response.body)
          json.should == {}
        end

        it "is successful" do
          get operation, :name => "testing"
          response.should be_success
        end
      end
    end
  end
end
