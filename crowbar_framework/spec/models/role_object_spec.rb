require 'spec_helper'

describe RoleObject do
  describe "finders" do
    describe "interface" do
      [
        :all,
        :find_roles_by_name,
        :find_roles_by_search,
        :find_role_by_name,
      ].each do |method|
        it "responds to #{method}" do
          RoleObject.should respond_to(method)
        end
      end
    end

    describe "all" do
      it "returns all roles" do
        roles = RoleObject.all
        roles.should_not be_empty
        roles.all? { |r| r.is_a?(RoleObject) }.should be_true
      end
    end

    describe "find_roles_by_name" do
      it "returns only matching roles" do
        roles = RoleObject.find_roles_by_name("crowbar")
        roles.should_not be_empty
        roles.all? { |r| r.name == "crowbar" }.should be_true
      end
    end

    describe "find_role_by_name" do
      it "returns only matching role" do
        role = RoleObject.find_role_by_name("crowbar")
        role.name.should == "crowbar"
      end
    end

    describe "active" do
      it "returns configured role names" do
        roles = RoleObject.active
        roles.should be_a(Array)
        roles.should_not be_empty
        roles.all? { |r| r.is_a?(String) }.should be_true
      end

      it "filters by barclamp if passed" do
        roles = RoleObject.active("crowbar")
        roles.should_not be_empty
        roles.all? { |r| r.match(/^crowbar/) }.should be_true
      end

      it "filters by barclamp and instance if passed" do
        roles = RoleObject.active("crowbar", "default")
        roles.should_not be_empty
        roles.all? { |r| r.match(/^crowbar/) }.should be_true
      end
    end

    describe "find_roles_by_search" do
      it "returns roles matching a search query" do
        roles = RoleObject.find_roles_by_search("name:*crowbar*")
        roles.all? { |r| r.name.include?("crowbar") }.should be_true
      end
    end
  end
end
