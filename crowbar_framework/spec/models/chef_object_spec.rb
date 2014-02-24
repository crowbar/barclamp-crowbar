# -*- encoding : utf-8 -*-
require 'spec_helper'

describe ChefObject do
  let(:chef_object) { ChefObject }

  after do
    if chef_object
      chef_object.class_eval { class_variable_set(:@@CrowbarDomain, nil) }
    end
  end

  describe "query chef" do
    it "returns new query" do
      chef_object.query_chef.should be_a(Chef::Search::Query)
    end

    it "returns empty node on failure" do
      Chef::Search::Query.stubs(:new).raises(StandardError)
      chef_object.query_chef.should be_a(Chef::Node)
    end
  end

  describe "cloud domain" do
    it "looks up the proposal object" do
      domain = "localdomain"
      bag = { :attributes => { :dns => { :domain => domain} } }
      ProposalObject.stubs(:find_proposal).once.returns(bag)
      domain.should == chef_object.cloud_domain
    end

    it "uses domainname if get proposal failed" do
      ProposalObject.stubs(:find_proposal).raises(StandardError)
      assert_equal %x{dnsdomainname}.strip, chef_object.cloud_domain
    end

    it "caches answers" do
      domain = "localdomain"
      bag = { :attributes => { :dns => { :domain => domain} } }

      ProposalObject.expects(:find_proposal).once.returns(bag)

      chef_object.cloud_domain
      chef_object.cloud_domain
    end
  end

  describe "crowbar_node" do
    it "looks up a node by name" do
      node = chef_object.crowbar_node("testing.crowbar.com")
      node.should be_a(Chef::Node)
      node.name.should == "testing.crowbar.com"
    end
  end

  describe "crowbar_data" do
    it "looks up a data bag in crowbar namespace" do
      data_bag = chef_object.crowbar_data("bc-crowbar-default")
      data_bag.should be_a(Chef::DataBagItem)
      data_bag.id.should == "bc-crowbar-default"
    end
  end
end

