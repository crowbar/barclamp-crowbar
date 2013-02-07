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
require 'json'

class JigMapModelTest < ActiveSupport::TestCase

  def setup
    @bc = Barclamp.create :name => "jig_map"
    @attrib = Attrib.add 'map_jig'
    @chef = Jig.find_or_create_by_name :name =>'chef', :type => 'BarclampChef::Jig', :order => 100
    @test = Jig.find_or_create_by_name :name =>'test', :type => 'BarclampCrowbar::Jig', :order => 200 
    assert_not_nil @chef
    assert_not_nil @test
  end
  
  test "add map from string" do
    count = BarclampChef::Jig.count + BarclampCrowbar::Jig.count
    chefmaps = @chef.maps(true).count
    testmaps = @test.maps(true).count
    maps = JigMap.add @attrib, @bc, "foo"
    assert_equal count, maps.count
    assert chefmaps < @chef.maps(true).count
    assert testmaps < @test.maps(true).count
    assert_equal maps[0].map, "foo"
    assert_equal maps[1].map, "foo"
  end
  
  test "add map from hash" do
    count = BarclampChef::Jig.count + BarclampCrowbar::Jig.count
    chefmaps = @chef.maps(true).count
    testmaps = @test.maps(true).count
    maps = JigMap.add @attrib, @bc, {:chef=>"bar"}
    assert_equal count, maps.count
    assert chefmaps < @chef.maps(true).count
    assert testmaps < @test.maps(true).count
    assert_equal maps[0].map, "bar"
    assert_equal maps[1].map, "bar"
  end
  
end

