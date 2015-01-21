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

require 'spec_helper'

describe NodesController do
  render_views

  before do
    NodeObject.any_instance.stubs(:system).returns(true)
  end

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

  describe "POST update" do
    before do
      NodeObject.stubs(:find_node_by_public_name).returns(nil)
      @node = NodeObject.find_node_by_name("admin")
    end

    describe "coming from the allocate form" do
      it "updates the node" do
        post :update, :name => @node.name, :submit => I18n.t('nodes.form.allocate'), :alias => "newname.crowbar.com", :public_name => "newname"
        flash[:notice].should == I18n.t('nodes.form.allocate_node_success')
        response.should redirect_to(node_path(@node.handle))
      end
    end

    describe "coming from the save form" do
      it "updates the node" do
        post :update, :name => @node.name, :submit => I18n.t('nodes.form.save'), :alias => "newname.crowbar.com", :public_name => "newname"
        flash[:notice].should == I18n.t('nodes.form.save_node_success')
        response.should redirect_to(node_path(@node.handle))
      end
    end

    describe "unknown submit" do
      it "sets the notice" do
        post :update, :name => @node.name, :submit => "i dont exist"
        flash[:notice].should match(/Unknown action/)
        response.should redirect_to(node_path(@node.handle))
      end
    end
  end

  describe "GET update" do
    it "raises unknown http method" do
      expect {
        get :update
      }.to raise_error(ActionController::UnknownHttpMethod)
    end
  end

  describe "GET list" do
    it "is successful" do
      get :list
      response.should be_success
    end
  end

  describe "POST bulk" do
    let(:admin) { NodeObject.find_node_by_name("admin") }
    let(:node) { NodeObject.find_node_by_name("testing") }

    it "redirects to nodes list on success if return param passed" do
      post :bulk, :node => { node.name => { "allocate" => true, "alias" => "newalias" } }, :return => "true"
      response.should redirect_to(nodes_list_path)
    end

    it "redirects to unallocated nodes list on success" do
      post :bulk, :node => { node.name => { "allocate" => true, "alias" => "newalias" } }
      response.should redirect_to(unallocated_list_path)
    end

    it "reports successful changes" do
      post :bulk, :node => { node.name => { "allocate" => true, "alias" => "newalias" }  }
      assigns(:report)[:failed].length.should == 0
      assigns(:report)[:success].should include(node.name)
    end

    it "reports duplicate alias nodes" do
      post :bulk, :node => { node.name => { "alias" => "newalias" }, admin.name => { "alias" => "newalias" } }
      assigns(:report)[:duplicate_alias].should == true
      assigns(:report)[:failed].should include(admin.name)
    end

    it "reports duplicate public name nodes" do
      post :bulk, :node => { node.name => { "public_name" => "newname" }, admin.name => { "public_name" => "newname" } }
      assigns(:report)[:duplicate_public].should == true
      assigns(:report)[:failed].should include(admin.name)
    end

    it "reports nodes for which update failed" do
      NodeObject.any_instance.stubs(:force_alias=).raises(StandardError)
      NodeObject.any_instance.stubs(:force_public_name=).raises(StandardError)

      post :bulk, :node => { node.name => { "allocate" => true, "alias" => "newalias" } }
      assigns(:report)[:failed].should include(node.name)

      post :bulk, :node => { node.name => { "allocate" => true, "public_name" => "newalias" } }
      assigns(:report)[:failed].should include(node.name)
    end
  end

  describe "GET families" do
    let(:node) { NodeObject.find_node_by_name("testing") }

    it "is successful" do
      get :families
      response.should be_success
    end

    it "sets populates @families with node descriptions" do
      get :families
      assigns(:families).keys.should include(node.family.to_s)
    end
  end

  describe "GET status" do
    it "is successful" do
      get :status, :format => "json"
      response.should be_success
    end

    it "renders error if fetch fails" do
      NodeObject.stubs(:all).raises(Errno::ECONNREFUSED)
      get :status, :format => "json"
      json = JSON.parse(response.body)
      json["error"].should_not be_empty
    end

    it "returns status of the nodes" do
      get :status, :format => "json"
      json = JSON.parse(response.body)

      json["nodes"]["admin"].keys.should include("status")
      json["nodes"]["testing"].keys.should include("status")

      json["nodes"]["admin"]["status"].should == "No Data (Off)"
      json["nodes"]["testing"]["status"].should == "Discovered"
    end
  end

  describe "GET show" do
    it "is successful" do
      get :show, :id => "testing"
      response.should be_success
    end

    describe "as json" do
      it "fails for missing node" do
        expect {
          get :show, :id => "missing", :format => "json"
        }.to raise_error(ActionController::RoutingError)
      end

      it "renders json" do
        get :show, :id => "testing", :format => "json"
        response.should be_success
      end
    end

    describe "as html" do
      it "redirects to dashboard for missing node" do
        get :show, :id => "missing"
        response.should redirect_to(nodes_path)
      end
    end
  end

  describe "POST hit" do
    it "returns 404 for missing node" do
      post :hit, :req => "identify", :id => "missing"
      response.should be_missing
    end

    it "returns 500 for invalid action" do
      post :hit, :req => "some nonsense", :id => "testing"
      response.should be_server_error
    end

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

  describe "POST group_change" do
    before do
      @node = NodeObject.find_node_by_name("testing")
    end

    it "returns not found for nonexistent node" do
      expect {
        new_group = "new_group"
        post :group_change, :id => "missing", :group => new_group
      }.to raise_error(ActionController::RoutingError)
    end

    it "assigns a node to a group" do
      NodeObject.stubs(:find_node_by_name).returns(@node)

      new_group = "new_group"
      post :group_change, :id => @node.name, :group => new_group

      @node.display_set?("group").should be true
      @node.group.should == new_group
    end

    it "sets node group to blank if 'automatic' passed" do
      NodeObject.stubs(:find_node_by_name).returns(@node)

      new_group = "automatic"
      post :group_change, :id => @node.name, :group => new_group

      @node.display_set?("group").should be false
      @node.group.should == "sw-#{@node.switch}"
    end
  end

  describe "GET attribute" do
    before do
      @node = NodeObject.find_node_by_name("testing")
    end

    # FIXME: maybe regular 404 would be better?
    it "raises for missing node" do
      expect {
        get :attribute, :name => "missing", :path => ["name"]
      }.to raise_error(ActionController::RoutingError)
    end

    it "raises for nonexistent attribute" do
      expect {
        get :attribute, :name => @node.name, :path => ["i dont exist"]
      }.to raise_error(ActionController::RoutingError)
    end

    it "renders complete node if no path passed" do
      get :attribute, :name => @node.name
      response.body.should == {:value => @node.to_hash}.to_json
    end

    it "looks up the attribute by path" do
      get :attribute, :name => @node.name, :path => ["name"]
      json = JSON.parse(response.body)
      json["value"].should == @node.name
    end
  end
end
