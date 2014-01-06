require File.expand_path(File.join(File.dirname(__FILE__), "..", "spec_helper"))

describe NodesController do
  integrate_views

  before do
    RoleObject.stubs(:find_role_by_name).returns(RoleObject.new(Chef::Role.new))
  end

  let(:chef_node) { n = Chef::Node.new; n.name "testing.example.com"; n }
  let(:node) { NodeObject.new(chef_node) }

  describe "GET index" do
    it "is successful" do
      NodeObject.stubs(:all).returns([node])

      with_constant("CHEF_ONLINE", true) do
        get :index
        response.should be_success
      end
    end
  end
end
