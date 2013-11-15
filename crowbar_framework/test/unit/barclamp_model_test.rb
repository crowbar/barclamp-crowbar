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
    @bc = Barclamp.find_by_name "crowbar"
  end
  
  test "Unique Name" do
    b = Barclamp.create! :name=>"nodup"
    assert_not_nil b
    e = assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordNotUnique) { Barclamp.create!(:name => "nodup") }
  end

  test "Check proections on illegal barclamp names" do
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name => "barclamp") }
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name => "docs") }
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name => "machines") }
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name => "users") }
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name => "support") }
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name => "application") }
  end

  test "Versions" do
    b = Barclamp.find_or_create_by_name(:name=>"crowbar")
    assert_not_nil b
    assert_equal [ "2.0" ], b.versions
  end

  test "Naming Conventions" do
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name=>"1123") }
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name=>"1foo") }
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name=>"Ille!gal") }
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name=>" nospaces") }
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name=>"no spaces") }
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name=>"nospacesatall ") }
  end
  
  test "barclamp type from name works" do
    name = "test"
    # this will have to be fixed when we merge in the engines code!
    namespace = "Barclamp#{name.camelize}"
    # we need to make sure that the barclamp is not in the DB
    testclass = "#{namespace}::Barclamp"
    # these routines look for the namespace & class, 
    m = Module::const_get(namespace) rescue nil
    c = m.const_get("Barclamp") rescue nil 
    if c
      bc = Barclamp.find_by_name(name) || Barclamp.import(name)
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

