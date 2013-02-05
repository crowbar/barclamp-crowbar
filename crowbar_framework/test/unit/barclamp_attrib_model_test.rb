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
    @template = BarclampInstance.create :name=>'template test', :barclamp_id=>@bc.id
    @template.add_role 'bamt'
    @bc.template_id = @template.id
    @bc.save
    assert_not_nil @bc, "we need to have a base barclamp"
    assert_not_nil @bc.template
    assert @bc.template.roles.count > 0
    @attrib = Attrib.find_or_create_by_name :name=>"unit_test", :barclamp_id=>@bc.id, :description=>'unit test target'
    assert_not_nil @attrib, "we need a base attrib"
    assert_equal 0, @attrib.barclamps.count, "and we start with no barclamps assigned"
  end  

  test "barclamp attrib has base attribs" do
    return
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
    ai = @bc.add_attrib({:name=>"relationtest"}, {:chef=>"unit_test"})
    a = ai.attrib
    assert_not_nil ai
    assert @bc.attribs.size > count
    assert @bc.attribs(true).size > count
    assert @bc.attribs(true).include? a
    assert @bc.jigs.include? Jig.find_by_name('chef')
  end  

  test "Attrib-Barclamp add attrib with map works" do
    h = "then look on top of the toilet tank"
    description = "fall through"
    order = 90
    a1 = @bc.add_attrib @attrib, @hint
    assert_not_nil a1, 'add attrib works'
    assert_equal @hint, a1.maps.first.map
    assert_equal @attrib.name, a1.attrib.name
    assert_equal @attrib.order, a1.attrib.order
  end
  
  test "Attrib-Barclamp add string attrib works" do
    bca = @bc.add_attrib "foo"
    assert_not_nil bca
    assert_equal "foo", bca.attrib.name
  end

  test "Attrib-Barclamp wrong type add" do
    e = assert_raise(ActiveRecord::RecordNotFound) {  bca = @bc.add_attrib(666) }
    assert_equal "Couldn't find Attrib with id=666", e.message
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
    count = @bc.template.attribs.count
    a = Attrib.add :name=>name
    a1 = @bc.add_attrib a, 'map/this'
    assert_not_nil a1
    assert_equal count+1, @bc.template.attribs(true).count    
    assert_equal a1.barclamp.id, @bc.id
    assert a1.attrib.barclamps.include?(@bc), "this is the new barclamp"
  end
end
