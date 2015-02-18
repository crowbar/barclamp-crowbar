require 'spec_helper'

describe ChefFinders do
  class FinderKlass
    class << self
      include ChefFinders
    end

    def initialize(item)
    end

    def self.chef_type
      "node"
    end

    def self.chef_class
      Chef::Node
    end
  end

  describe "build_query" do
    it "concatenates single attribute and its value" do
      FinderKlass.build_query(:id => :something).should == "id:something"
    end

    it "joins multiple attributes with and" do
      FinderKlass.build_query(:foo => :else, :id => :something).should == "foo:else and id:something"
    end

    it "works with or as a string" do
      FinderKlass.build_query(:or => :something).should == "or:something"
    end

    it "works with or as a single hash" do
      FinderKlass.build_query(:or => { :foo => :else, :id => :something}).should == "(foo:else or id:something)"
    end

    it "works with nested ors" do
      query = FinderKlass.build_query(:transitions => true, :or => { :transition_list => [:all, :running] })
      query.should include("(transition_list:all or transition_list:running)")
      query.should include("transitions:true")
    end
  end

  describe "after_find_filter" do
    it "is called after search" do
      FinderKlass.expects(:after_find_filter).once
      FinderKlass.all
    end
  end

  describe "dynamic finders" do
    it "resolves to regular finder" do
      FinderKlass.expects(:where).with({:name => "test"}).once
      FinderKlass.find_all_by_name("test")
    end

    it "accepts multiple params separated by and" do
      FinderKlass.expects(:where).with({:name => "test", :foo => "bar"}).once
      FinderKlass.find_all_by_name_and_foo("test", "bar")
    end

    it "raises argument error w/ too many arguments" do
      expect {
        FinderKlass.find_all_by_name("test", "bar", :raw => true)
      }.to raise_error(ArgumentError)

      expect {
        FinderKlass.find_all_by_name("test", {:raw => true}, "bar")
      }.to raise_error(ArgumentError)

      expect {
        FinderKlass.find_all_by_name("test", "bar")
      }.to raise_error(ArgumentError)
    end

    it "raises argument error w/ too few arguments" do
      expect {
        FinderKlass.find_all_by_name_and_foo("test")
      }.to raise_error(ArgumentError)
    end

    it "returns the collection" do
      FinderKlass.stubs(:where).returns(["a", "b"])
      FinderKlass.find_all_by_name("foo").should == ["a", "b"]
    end

    it "returns the first record if called w/o all" do
      FinderKlass.stubs(:where).returns(["a", "b"])
      FinderKlass.find_by_name_and_place("foo", "bar").should == "a"
    end

    it "raises w/ missing record when called with !" do
      FinderKlass.stubs(:where).returns(nil)
      expect {
        FinderKlass.find_by_name_and_place!("foo", "bar")
      }.to raise_error(ChefFinders::RecordNotFound)
    end

    it "raises w/ empty collection when called with !" do
      FinderKlass.stubs(:where).returns([])
      expect {
        FinderKlass.find_all_by_name_and_place!("foo", "bar")
      }.to raise_error(ChefFinders::RecordNotFound)
    end
  end
end
