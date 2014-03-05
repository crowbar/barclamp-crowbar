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

describe CrowbarController do
  render_views

  before do
    CrowbarService.any_instance.stubs(:apply_role).returns([200, "OK"])
  end

  describe "GET index" do

    it "renders list of services" do
      get :index
      response.should be_success
      assigns(:modules).map { |s| s[0] }.should include("crowbar")
    end

    it "renders list of active roles as json" do
      get :index, :format => "json"
      response.should be_success
      json = JSON.parse(response.body)
      json.should == assigns(:service_object).list_active.last
    end
  end

  describe "GET barclamp_index" do
    # FIXME: missing view file, removed in b8430b867d09c4aa5d6502176365958c49691563
=begin
    it "renders list of all barclamps" do
      get :barclamp_index
      response.should be_success
      assigns(:barclamps).should include("crowbar")
    end
=end

    it "returns list of barclamp names as json" do
      get :barclamp_index, :format => "json"
      response.should be_success
      json = JSON.parse(response.body)
      json.should include("crowbar")
    end
  end

  describe "GET versions" do
    it "returns json with versions" do
      get :versions
      json = JSON.parse(response.body)
      json["versions"].should_not be_empty
    end

    it "returns plain text message if version fetching fails" do
      CrowbarService.any_instance.stubs(:versions).returns([404, "Not found"])
      get :versions
      response.should be_missing
      response.body.should == "Not found"
    end
  end

  describe "POST transition" do
    it "transitions the node into desired state" do
      RoleObject.stubs(:find_roles_by_search).returns([])
      post :transition, :barclamp => "crowbar", :id => "default", :state => "discovering", :name => "testing"
      response.should be_success
    end

    it "returns plain text message if transitioning fails" do
      CrowbarService.any_instance.stubs(:transition).returns([500, "error"])
      post :transition, :barclamp => "crowbar", :id => "default", :state => "discovering", :name => "testing"
      response.should be_server_error
      response.body.should == "error"
    end

    it "returns node as a hash on success when passed a name" do
      CrowbarService.any_instance.stubs(:transition).returns([200, { :name => "testing" } ])
      post :transition, :barclamp => "crowbar", :id => "default", :state => "discovering", :name => "testing"
      response.should be_success
      json = JSON.parse(response.body)
      json["name"].should == "testing.crowbar.com"
    end

    it "returns node as a hash on success when passed a node (backward compatibility)" do
      CrowbarService.any_instance.stubs(:transition).returns([200, NodeObject.find_node_by_name("testing").to_hash ])
      post :transition, :barclamp => "crowbar", :id => "default", :state => "discovering", :name => "testing"
      response.should be_success
      json = JSON.parse(response.body)
      json["name"].should == "testing.crowbar.com"
    end
  end

  describe "GET show" do
    describe "format json" do
      it "returns plain text message if show fails" do
        CrowbarService.any_instance.stubs(:show_active).returns([500, "Error"])
        post :show, :id => "default", :format => "json"
        response.should be_server_error
        response.body.should == "Error"
      end

      it "returns a json describing the instance" do
        get :show, :id => "default", :format => "json"
        response.should be_success
        json = JSON.parse(response.body)
        json["deployment"].should_not be_nil
      end
    end

    describe "format html" do
      it "is successful" do
        get :show, :id => "default"
        response.should be_success
      end

      it "redirects to propsal path on failure" do
        CrowbarService.any_instance.stubs(:show_active).returns([500, "Error"])
        get :show, :id => "default"
        response.should redirect_to(proposal_show_path(:controller => "crowbar", :id => "default"))
      end
    end
  end

  describe "GET elements" do
    it "returns plain text message if elements fails" do
      CrowbarService.any_instance.stubs(:elements).returns([500, "Error"])
      get :elements
      response.should be_server_error
      response.body.should == "Error"
    end

    it "returns a json with list of assignable roles" do
      get :elements
      response.should be_success
      json = JSON.parse(response.body)
      json.should be_a(Array)
      json.should_not be_empty
    end
  end

  describe "GET element_info" do
    it "returns plain text message if element_info fails" do
      CrowbarService.any_instance.stubs(:element_info).returns([500, "Error"])
      get :element_info
      response.should be_server_error
      response.body.should == "Error"
    end

    it "returns a json with list of assignable nodes for an element" do
      get :element_info, :id => "dns-client"
      response.should be_success
      json = JSON.parse(response.body)
      nodes = ["admin.crowbar.com", "testing.crowbar.com"]
      json.sort.should == nodes.sort
    end
  end

  describe "GET proposals" do
    it "returns plain text message if proposals fails" do
      CrowbarService.any_instance.stubs(:proposals).returns([500, "Error"])
      get :proposals
      response.should be_server_error
      response.body.should == "Error"
    end

    it "is successful" do
      get :proposals
      response.should be_success
    end

    it "returns a list of proposals for a given instance" do
      get :proposals, :format => "json"
      json = JSON.parse(response.body)
      response.should be_success
      json.should == ["default"]
    end
  end

  describe "DELETE delete" do
    before do
     CrowbarService.any_instance.stubs(:system).returns(true)
    end

    it "deletes and deactivates the instance" do
      CrowbarService.any_instance.expects(:destroy_active).with("default").once
      delete :delete, :name => "default"
    end

    it "sets appropriate flash message" do
      CrowbarService.any_instance.stubs(:destroy_active).returns([200, "Yay!"])
      delete :delete, :name => "default"
      flash[:notice].should == I18n.t('proposal.actions.delete_success')
    end

    it "redirects to barclamp module on success" do
      delete :delete, :name => "default"
      response.should redirect_to(barclamp_modules_path(:id => "crowbar"))
    end

    it "returns 500 on failure for json" do
      CrowbarService.any_instance.stubs(:destroy_active).returns([500, "Error"])
      delete :delete, :name => "default", :format => "json"
      response.should be_server_error
      response.body.should == "Error"
    end

    it "sets flash on failure for html" do
      CrowbarService.any_instance.stubs(:destroy_active).returns([500, "Error"])
      delete :delete, :name => "default"
      response.should be_redirect
      flash[:alert].should_not be_nil
    end
  end

  describe "PUT proposal_create" do
    let(:proposal) { ProposalObject.find_proposal("crowbar", "default") }

    # We don't validate_proposal_after_save as freshly created proposals can be
    # missing nodes. However, this is ok, as users will assign roles to them
    # later.
    before(:each) do
      CrowbarService.any_instance.expects(:validate_proposal).at_least_once
      CrowbarService.any_instance.expects(:validate_proposal_elements).returns(true).at_least_once
    end

    it "validates a proposal" do
      put :proposal_create, :name => "nonexistent"
    end
  end

  describe "proposal updates" do
    let(:proposal) { ProposalObject.find_proposal("crowbar", "default") }

    before(:each) do
      CrowbarService.any_instance.expects(:validate_proposal).at_least_once
      CrowbarService.any_instance.expects(:validate_proposal_elements).returns(true).at_least_once
      CrowbarService.any_instance.expects(:validate_proposal_after_save).at_least_once
    end

    describe "POST proposal_commit" do
      it "validates a proposal" do
        post :proposal_commit, :id => "default"
      end
    end

    describe "PUT proposal_update" do
      it "validates a proposal from command line" do
        prop = JSON.parse(proposal.to_json, :create_additions => false)["item"]["raw_data"].merge("id" => "default")
        put :proposal_update, prop
      end

      it "validates a proposal from the UI" do
        put :proposal_update, :name => "default", :barclamp => "crowbar", :submit => I18n.t('barclamp.proposal_show.save_proposal'), :proposal_attributes => "{}", :proposal_deployment => "{}"
      end
    end
  end
end

