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
 
class BarclampAttribModelTest < ActiveSupport::TestCase

  # tests the relationship between nodes and attributes)
  def setup
    @hint = "look under the cushions on the couch"
    # Attrib depends on crowbar barclamp - we need to find/create it first
    @crowbar = Barclamp.find_or_create_by_name :name=>"crowbar"
    assert_not_nil @crowbar, "we need to have a crowbar barclamp"
    @bc = Barclamp.find_or_create_by_name :name=>"test_units"
    assert_not_nil @bc, "we need to have a base barclamp"
    @attrib = Attrib.find_or_create_by_name :name=>"unit_test", :barclamp_id=>@bc.id, :description=>'unit test target', :hint=>@hint
  end  

  test "attribute without barclamp defaults to crowbar" do
    assert_not_nil @crowbar, "we need the crowbar barclamp"
    count = @crowbar.attribs.size
    a = Attrib.find_or_create_by_name :name=>"default_to_crowbar"
    assert_not_nil a
    assert_not_nil a.barclamp
    assert_equal @crowbar.name, a.barclamp.name
    assert_equal @crowbar.id, a.barclamp.id
    cb = Barclamp.find @crowbar.id
    assert_equal count+1, cb.attribs.size, "we we have another attrib"
  end
  
  test "Barclamp-Attrib Relation" do
    count = @bc.attribs.size
    a = Attrib.find_or_create_by_name :name=>"relationtest", :barclamp_id=>@bc.id
    assert @bc.attribs.size > count
    b = Barclamp.find @bc.id
    assert b.attribs.size > count
    assert b.attribs.include? a
  end  

  test "Attrib-Barclamp has hint" do
    h = "then look on top of the toilet tank"
    assert_equal @hint, @attrib.hint
    @attrib.hint = h
    @attrib.save
    a = Attrib.find @attrib.id
    assert_not_nil a
    assert_equal h, a.hint
    assert_not_equal @hint, a.hint
  end
  

  test "Barclamp addAttrib creates attributes" do
    name = "add_attrib"
    desc = "mashed potatoes"
    hint = "in the cabinet"
    order = 6
    props = {:name=>name, :description=>desc, :hint=>hint, :order=>order}
    assert_not_nil @bc
    a = @bc.add_attrib props
    assert_not_nil a
    assert_equal name, a.name
    assert_equal desc, a.description
    assert_equal order, a.order
    assert_equal hint, a.hint
    assert_equal @bc.id, a.barclamp_id
  end
  
  test "Barclamp addAttrib updates attributes" do
    name = @attrib.name
    desc = "mashed potatoes"
    hint = "in the cabinet"
    order = 6
    props = {:name=>name, :description=>desc, :hint=>hint, :order=>order}
    assert_not_nil @bc
    a = @bc.add_attrib props
    assert_not_nil a
    assert_equal a.id, @attrib.id 
    assert_equal name, a.name
    assert_equal desc, a.description
    assert_equal order, a.order
    assert_equal hint, a.hint
    assert_not_equal @hint, a.hint
    assert_equal @bc.id, a.barclamp_id
  end
  
  test "Barclamp addAttrib requires name" do
    props = {:description=>"desc", :hint=>"hint"}
    e = assert_raise(NameError) { @bc.add_attrib(props) }
    assert_equal "uncaught throw `Requires Name'", e.message
  end
  
  test "Barclamp addAttrib cannot reassign barclamp" do
    name = "dontmoveme"
    bc1 = Barclamp.create :name=>"unittest_foo"
    assert_not_nil bc1
    a1 = bc1.add_attrib :name=>name
    assert_not_nil a1
    e = assert_raise(NameError) { @bc.add_attrib(:name=>name) }
    assert_equal "uncaught throw `Cannot Reassign Barclamp'", e.message
    a1_again = Attrib.find a1.id
    assert_equal bc1.id, a1.barclamp_id, "confirm it did not change"
  end

  test "Barclamp addAttrib can reassign from crowbar barclamp" do
    name = "domoveme"
    a1 = @crowbar.add_attrib :name=>name
    assert_not_nil a1
    a2 = @bc.add_attrib :name=>name
    assert_not_nil a2
    assert_not_equal @crowbar.id, a2.barclamp_id, "this is not crowbar barclamp"
    assert_equal a1.id, a2.id, "this is the same attrib"
    assert_equal @bc.id, a2.barclamp_id, "this is the new barclamp"
  end
  
  test "Barclamp Register creates attributes" do
    assert false, "test not created"
  end

  test "Barclamp run_data create mode data" do
    assert false, "test not created"
  end

  
end

