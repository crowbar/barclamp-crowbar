# Copyright 2013, Dell 
# 
# Licensed under the Apache License, Version 2.0 (the "License"); 
# you may not use this file except in compliance with the License. 
# You may obtain a copy of the License at 
# 
#  http://www.apache.org/licenses/LICENSE-2.0 
# 
# Unless required by applicable law or agreed to in writing, software 
# distributed under the License is distributed on an "AS IS" BASIS, 
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
# See the License for the specific language governing permissions and 
# limitations under the License. 
# 
require 'test_helper'
 
class BarclampModelTest < ActiveSupport::TestCase

  def setup
    # we need to make sure that we have crowbar role
    if Barclamp.find_by_name('crowbar').nil?
      Barclamp.import_1x 'crowbar'
    end
    b = Barclamp.find_by_name "crowbar"
  end
  
  test "Unique Name" do
    b = Barclamp.create! :name=>"nodup"
    assert_not_nil b
    e = assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordNotUnique, SQLite3::ConstraintException) { Barclamp.create!(:name => "nodup") }
  end

  test "Check proections on illegal barclamp names" do
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name => "barclamp") }
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name => "docs") }
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name => "machines") }
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name => "users") }
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name => "support") }
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name => "application") }
  end
  
  test "Roles Relation" do
    b = Barclamp.find_or_create_by_name(:name=>"crowbar")
    assert_not_nil b

    r = b.roles

    r1 = Role.find_by_name("crowbar")
    r2 = Role.find_private

    assert_equal 2, r.size
    assert r.include? r1
    assert r.include? r2
  end

  test "Template Relation" do
    b = Barclamp.find_or_create_by_name(:name=>"crowbar")
    assert_not_nil b
    t = b.template
    assert_equal "Crowbar template", t.name, "this comes from the model.barclamp.template localization"
    assert_instance_of BarclampInstance, t
    assert_equal "private", t.roles.first.name
    assert_equal "crowbar", t.roles.second.name
  end

  test "Proposals empty" do
    b = Barclamp.find_or_create_by_name(:name=>"crowbar")
    assert_not_nil b
    t = b.proposals

    assert_equal [], t
  end

  test "Active Proposals empty" do
    b = Barclamp.find_or_create_by_name(:name=>"crowbar")
    assert_not_nil b
    t = b.active_proposals

    assert_equal [], t
  end

  test "Operations function" do
    b = Barclamp.find_or_create_by_name(:name=>"crowbar")
    assert_instance_of(CrowbarService, b.operations)
  end

  test "Versions" do
    b = Barclamp.find_or_create_by_name(:name=>"crowbar")
    assert_not_nil b
    assert_equal [ "1.0" ], b.versions
  end

  test "Naming Conventions" do
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name=>"1123") }
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name=>"1foo") }
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name=>"Ille!gal") }
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name=>" nospaces") }
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name=>"no spaces") }
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name=>"nospacesatall ") }
  end
  
  test "Get Roles by Order" do
    b = Barclamp.find_or_create_by_name("crowbar")
    assert_not_nil b
    ro = b.roles
    assert_not_nil ro
    begin
      assert_equal 2, ro.length
      assert_equal 'private', ro.first.name
      assert_equal 'crowbar', ro.second.name
    rescue
      
      flunk("Exception inside get roles due to missing roles by order")
    end
  end

  test "barclamp type from name works" do
    name = "test"
    # we need to make sure that the barclamp is not in the DB
    testclass = name.camelize+"::Barclamp"
    if File.exist? File.join('app', 'models', name, 'barclamp.rb')
      id = Barclamp.find_by_name name
      Barclamp.delete id
      bc = Barclamp.find_or_create_by_name :name=>name
      assert_not_nil bc
      assert_equal name, bc.name
      assert_equal testclass, bc.type
    else
      puts "skipping barclamp_model_test:barclamp type from name works because the #{testclass} file was not found"
      assert true, "skip this test, we don't have the #{testclass} installed"
    end
  end

  test "barclamp falls back to framework type if missing" do
    name = "doesnotexist"
    bc = Barclamp.create :name=>name
    assert_not_nil bc
    assert_equal name, bc.name
    assert_equal "BarclampFramework", bc.type
  end

end

