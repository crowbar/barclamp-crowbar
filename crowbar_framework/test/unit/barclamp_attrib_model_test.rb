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
    @bc = Barclamp.find_or_create_by_name :name=>"test"
    assert_not_nil @bc, "we need to have a base barclamp"
    @attrib = Attrib.find_or_create_by_name :name=>"unit_test", :barclamp_id=>@bc.id, :description=>'unit test target'
    assert_not_nil @attrib, "we need a base attrib"
    assert_equal 0, @attrib.barclamps.count, "and we start with no barclamps assigned"
  end  

  test "barclamp attrib has base attribs" do
    return
    # TODO THIS NEEDS TO CHANGE
    o = AttribInstance.find_or_create_by_barclamp_instance_id_and_attrib_id :barclamp_instance_id=>@bc.id, :attrib_id => @attrib.id
    @bc.add_attrib @attrib
    assert_not_nil o
    o.description=@hint
    o.order=666
    o.save
    bca = AttribInstance.find_or_create_by_barclamp_instance_id_and_attrib_id :barclamp_instance_id=>@bc.id, :attrib_id => @attrib.id
    assert_equal @bc.id, bca.barclamp.id
    assert_equal @bc.id, bca.barclamp_id
    assert_equal @attrib.id, bca.attrib.id
    assert_equal @attrib.id, bca.attrib_id
    assert_equal @hint, bca.description
    assert_equal 666, bca.order    
  end
  
  test "attribute without barclamp is ok" do
    assert_not_nil @crowbar, "we need the crowbar barclamp"
    count = @crowbar.attribs.size
    a = Attrib.find_or_create_by_name :name=>"default_to_crowbar"
    assert_not_nil a
    assert_not_nil a.barclamps
    assert_equal 0, a.barclamps.count
  end
  
  test "Barclamp-Attrib Relation" do
    count = @bc.attribs.size
    bca = @bc.add_attrib :name=>"relationtest"
    assert_not_nil bca
    a = bca.attrib
    assert_not_nil a
    assert @bc.attribs.size > count
    b = Barclamp.find @bc.id
    assert b.attribs.size > count
    assert b.attribs.include? a
  end  

  test "Attrib-Barclamp add attrib works" do
    h = "then look on top of the toilet tank"
    description = "fall through"
    order = 90
    bca = @bc.add_attrib(@attrib, {:description=>@hint, :order=>999})
    assert_not_nil bca, 'add attrib works'
    assert_equal @hint, bca.description
    assert_equal 999, bca.order
    assert_equal @attrib.name, bca.attrib.name
    assert_equal @attrib.order, bca.attrib.order
    bca.description = h
    bca.order = 666
    bca.save
    a = BarclampAttrib.find bca.id
    assert_not_nil a
    assert_equal h, a.description
    assert_not_equal @hint, a.description
    assert_equal 666, a.order
    assert_not_equal 999, a.order
    assert_equal @bc.id, a.barclamp_id
    assert_equal @attrib.id, a.attrib_id
  end
  
  test "Attrib-Barclamp add string attrib works" do
    bca = @bc.add_attrib "foo"
    assert_not_nil bca
    assert_equal "foo", bca.attrib.name
  end

  test "Attrib-Barclamp wrong type add" do
    e = assert_raise(NameError, ArgumentError) {  bca = @bc.add_attrib(666) }
    assert_equal "uncaught throw `barclamp.add_attrib cannot use Fixnum to create from attribute: 666'", e.message
  end
  
  test "Barclamp addAttrib requires name not description or order" do
    if RUBY_VERSION == '1.8.7'
      first_quote  = '`'
      second_quote = "'"
    else
      first_quote  = '"'
      second_quote = '"'
    end
    e = assert_raise(NoMethodError, NameError, ArgumentError) { @bc.add_attrib(nil) }
    assert_equal "uncaught throw #{first_quote}barclamp.add_attrib requires Attrib object or hash with :name#{second_quote}", e.message
    e = assert_raise(NoMethodError, NameError, ArgumentError) { @bc.add_attrib :description=>"foo" }
    assert_equal "uncaught throw #{first_quote}barclamp.add_attrib requires attribute :name#{second_quote}", e.message
  end
  
  test "Barclamp addAttrib adds to barclamp list" do
    name = "domoveme"
    count = @bc.attribs.count
    a = Attrib.find_or_create_by_name :name=>name
    a1 = @bc.add_attrib a
    assert_not_nil a1
    bc = Barclamp.find @bc.id
    assert_equal count+1, bc.attribs.count    
    assert_equal a1.barclamp.id, @bc.id
    assert a1.attrib.barclamps.include?(@bc), "this is the new barclamp"
  end
end
