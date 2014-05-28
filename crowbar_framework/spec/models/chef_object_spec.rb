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
end
