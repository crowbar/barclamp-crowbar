# -*- encoding : utf-8 -*-
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
      get :index
      response.should be_success
    end

    context "with some nodes" do
      it "renders json" do
        get :index
        JSON.parse(response.body).should be_a(Hash)
      end

      it "results in filled nodes hash" do
        get :index
        json = JSON.parse(response.body)

        json["nodes"].should be_a(Array)
        json["nodes"].should_not be_empty
      end

      it "contains name keys" do
        get :index
        json = JSON.parse(response.body)
        node = json["nodes"].first

        node.should be_a(Hash)
        node.should have_key("name")
      end

      it "contains alias keys" do
        get :index
        json = JSON.parse(response.body)
        node = json["nodes"].first

        node.should be_a(Hash)
        node.should have_key("alias")
      end
    end

    context "without nodes" do
      before do
        NodeObject.stubs(:find_all_nodes).returns([])
      end

      it "renders json" do
        get :index
        JSON.parse(response.body).should be_a(Hash)
      end

      it "results in empty nodes hash" do
        get :index
        json = JSON.parse(response.body)

        json["nodes"].should be_a(Array)
        json["nodes"].should be_empty
      end
    end
  end

  describe "GET show" do
    it "is successful" do
      get :show, :name => "testing"
      response.should be_success
    end

    it "renders json" do
      get :show, :name => "testing"
      JSON.parse(response.body).should be_a(Hash)
    end

    context "for existent node" do
      it "fetches with name" do
        get :show, :name => "testing"
        json = JSON.parse(response.body)

        json["name"].should be_a(String)
        json["name"].should eql("testing.crowbar.com")
      end

      it "works with fqdn" do
        get :show, :name => "testing.crowbar.com"
        json = JSON.parse(response.body)

        json["name"].should be_a(String)
        json["name"].should eql("testing.crowbar.com")
      end
    end

    context "for non-existent node" do
      it "renders 404" do
        get :show, :name => "nonexistent"
        response.status.should eql("404 Not Found")
      end
    end
  end

  describe "POST rename" do
    context "for existent node" do
      it "renames a node to tester" do
        NodeObject.any_instance.expects(:alias=).with("tester").once
        NodeObject.any_instance.expects(:save).once

        post :rename, :name => "testing", :alias => "tester"
        response.status.should eql("200 OK")
      end
    end

    context "for non-existent node" do
      it "renders 404" do
        post :rename, :name => "nonexistent"
        response.status.should eql("404 Not Found")
      end
    end
  end

  describe "DELETE delete" do
    context "for existent node" do
      it "invokes delete" do
        NodeObject.any_instance.expects(:delete).once

        delete :delete, :name => "testing"
        response.status.should eql("200 OK")
      end
    end

    context "for non-existent node" do
      it "renders 404" do
        delete :delete, :name => "nonexistent"
        response.status.should eql("404 Not Found")
      end
    end
  end

  [
    :reinstall,
    :update,
    :reset,
    :identify,
    :shutdown,
    :reboot,
    :poweron,
    :allocate
  ].each do |action|
    describe "POST #{action}" do
      context "for existent node" do
        it "invokes #{action}" do
          NodeObject.any_instance.expects(action).once

          post action, :name => "testing"
          response.status.should eql("200 OK")
        end
      end

      context "for non-existent node" do
        it "renders 404" do
          post action, :name => "nonexistent"
          response.status.should eql("404 Not Found")
        end

        it "prevents #{action}" do
          NodeObject.any_instance.expects(action).never
        end
      end
    end
  end
end

