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

describe MachinesController do
  before do
    NodeObject.any_instance.stubs(:system).returns(true)
  end

  describe "GET index" do
    before do
      File.stubs(:exist?).returns(true)
    end

    it "is successful" do
      get :index, :format => "json"
      expect(response).to have_http_status(:ok)
    end

    context "with some nodes" do
      it "renders json" do
        get :index, :format => "json"
        expect(JSON.parse(response.body)).to be_a(Hash)
      end

      it "results in filled nodes hash" do
        get :index, format: "json"
        json = JSON.parse(response.body)

        expect(json["nodes"]).to be_a(Array)
        expect(json["nodes"]).not_to be_empty
      end

      it "contains name keys" do
        get :index, :format => "json"
        json = JSON.parse(response.body)
        node = json["nodes"].first

        expect(node).to have_key("name")
      end

      it "contains alias keys" do
        get :index, :format => "json"
        json = JSON.parse(response.body)
        node = json["nodes"].first

        expect(node).to have_key("alias")
      end
    end

    context "without nodes" do
      before do
        NodeObject.stubs(:find_all_nodes).returns([])
      end

      it "renders json" do
        get :index, :format => "json"
        expect(JSON.parse(response.body)).to be_a(Hash)
      end

      it "results in empty nodes hash" do
        get :index, :format => "json"
        json = JSON.parse(response.body)

        expect(json["nodes"]).to be_a(Array)
        expect(json["nodes"]).to be_empty
      end
    end
  end

  describe "GET show" do
    it "is successful" do
      get :show, :name => "testing", :format => "json"
      expect(response).to have_http_status(:ok)
    end

    it "renders json" do
      get :show, :name => "testing", :format => "json"
      expect(JSON.parse(response.body)).to be_a(Hash)
    end

    context "for existent node" do
      it "fetches with name" do
        get :show, :name => "testing", :format => "json"
        json = JSON.parse(response.body)

        expect(json["name"]).to eq("testing.crowbar.com")
      end

      it "works with fqdn" do
        get :show, :name => "testing.crowbar.com", :format => "json"
        json = JSON.parse(response.body)

        expect(json["name"]).to eq("testing.crowbar.com")
      end
    end

    context "for non-existent node" do
      it "renders 404" do
        get :show, :name => "nonexistent", :format => "json"
        expect(response).to have_http_status(:not_found)
      end
    end
  end

  describe "POST role" do
    context "for existent node" do
      it "assignes role compute" do
        NodeObject.any_instance.expects(:intended_role=).with("compute").once

        post :role, name: "testing", role: "compute", format: "json"
        expect(response).to have_http_status(:ok)
      end
    end

    it "return 404 (not found) http status when node does not exists" do
      post :role, name: "nonexistent", format: "json"
      expect(response).to have_http_status(:not_found)
    end

    it "return 422 (unprocessable_entity) http status when save fails" do
      NodeObject.any_instance.stubs(:save).returns(false)
      NodeObject.any_instance.expects(:intended_role=).with("compute").once

      post :role, name: "testing", role: "compute", format: "json"

      expect(response).to have_http_status(:unprocessable_entity)
    end
  end

  describe "POST rename" do
    context "for existent node" do
      it "renames a node to tester" do
        NodeObject.any_instance.expects(:alias=).with("tester").once

        post :rename, name: "testing", alias: "tester", format: "json"
        expect(response).to have_http_status(:ok)
      end
    end

    it "return 404 (not found) http status when node does not exists" do
      post :rename, name: "nonexistent", format: "json"
      expect(response).to have_http_status(:not_found)
    end

    it "return 422 (unprocessable_entity) http status when save fails" do
      NodeObject.any_instance.stubs(:save).returns(false)
      NodeObject.any_instance.expects(:alias=).with("tester").once

      post :rename, name: "testing", alias: "tester", format: "json"

      expect(response).to have_http_status(:unprocessable_entity)
    end
  end

  [
    :update,
    :identify
  ].each do |action|
    describe "POST #{action}" do
      context "for existent node" do
        it "invokes #{action}" do
          NodeObject.any_instance.expects(action).once

          post action, name: "testing", format: "json"
          expect(response).to have_http_status(:ok)
        end
      end

      context "for non-existent node" do
        it "return 404 (not found) http status" do
          post action, name: "nonexistent", format: "json"
          expect(response).to have_http_status(:not_found)
        end

        it "prevents #{action}" do
          NodeObject.any_instance.expects(action).never
        end
      end
    end
  end

  [
    :reinstall,
    :reset,
    :shutdown,
    :reboot,
    :poweron,
    :powercycle,
    :poweroff,
    :allocate,
    :delete
  ].each do |action|
    describe "POST #{action}" do
      context "for existent node" do
        it "invokes #{action}" do
          NodeObject.any_instance.expects(action).once

          post action, name: "testing", format: "json"
          expect(response).to have_http_status(:ok)
        end

        it "return 403 (forbidden) http status for admin node" do
          NodeObject.any_instance.stubs(:admin?).returns(true)
          post action, name: "testing", format: "json"
          expect(response).to have_http_status(:forbidden)
        end
      end

      context "for non-existent node" do
        it "renders 404" do
          post action, :name => "nonexistent", :format => "json"
          expect(response).to have_http_status(:not_found)
        end

        it "prevents #{action}" do
          NodeObject.any_instance.expects(action).never
        end
      end
    end
  end
end
