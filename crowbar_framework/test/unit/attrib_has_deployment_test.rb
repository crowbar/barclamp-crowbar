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

class AttribHasDeploymentTest < ActiveSupport::TestCase

  HAS_NODE = BarclampCrowbar::AttribHasDeployment::HASDEPLOYMENT_NAME

  # tests the relationship between nodes and attributes
  def setup
    # setup node w/ attribute
    @crowbar = Barclamp.import 'crowbar'
    @test = Barclamp.import 'test'
    assert_not_nil @crowbar
    assert_not_nil @crowbar.template
    assert_equal 2, @crowbar.template.roles.count
    @role = @crowbar.template.public_roles.first
    assert_not_nil @role
    
    # Ruby 1.8 and 1.9 raise different exceptions in this case, so handle it
    # accordingly. Simplify once we remove 1.8 support.
    @error_class = (RUBY_VERSION == '1.8.7') ? NameError : ArgumentError
  end

  test "classes are right" do
    assert_equal BarclampCrowbar::AttribHasDeployment, Role::HAS_DEPLOYMENT 
  end
  
  test "special attrib is used correctly on create" do
    attrib = @role.require_deployment 'test'
    assert_instance_of BarclampCrowbar::AttribHasDeployment, attrib
    a = attrib.attrib_type
    assert_instance_of AttribType, a
    assert_equal BarclampCrowbar::AttribHasDeployment::HASDEPLOYMENT_NAME, a.name
    assert_equal I18n.t('model.attribs.role.has_deployment'), a.description
    assert_equal 999998, a.order
  end
  
  test "attrib id is read only" do
    attrib = @role.require_deployment 'test'
    current_attrib = attrib.attrib_type_id
    a = AttribType.add 'foo'
    attrib.attrib_type_id = a.id
    assert_equal a.id, attrib.attrib_type_id
    attrib.save
    assert_equal current_attrib, attrib.attrib_type_id
    assert_not_equal a.id, attrib.attrib_type.id
  end
  
  test "deployment is created if missing" do
    t = Barclamp.find_by_name 'test'
    assert_equal 0, t.deployments(true).count
    attrib = @role.require_deployment 'test', 'foo'
    assert_equal 1, t.deployments(true).size
    assert_equal 'foo', t.deployments.first.name
  end

  test "deployment finds first if none given and only 1 allowed" do
    t = Barclamp.find_by_name 'test'
    t.allow_multiple_deployments = false
    t.save
    t.create_deployment 'bar'
    assert_equal 1, t.deployments(true).count
    attrib = @role.require_deployment 'test'
    assert_equal 1, t.deployments(true).size
    assert_equal 'bar', attrib.deployment.name
  end

  test "deployment creates new if none given and multiple allowed" do
    t = Barclamp.find_by_name 'test'
    t.allow_multiple_deployments = true
    t.save
    t.create_deployment
    assert_equal 1, t.deployments(true).count 
    attrib = @role.require_deployment 'test', 'bar'
    assert_equal 2, t.deployments(true).count
    assert_equal 'bar', attrib.deployment.name
  end
  
  test "deployment finds match if given" do
    t = Barclamp.find_by_name 'test'
    t.create_deployment 'bar'
    assert_equal 1, t.deployments(true).count
    attrib = @role.require_deployment 'test', 'bar'
    assert_equal 1, t.deployments(true).count
    assert_equal 'bar', attrib.deployment.name
  end

  test "role is set correct" do
    t = Barclamp.find_by_name 'test'
    r = t.template.public_roles.first.name
    attrib = @role.require_deployment 'test', 'foo', r
    attrib.save
    assert_equal 'test', attrib.deployment.barclamp.name
    assert_equal 'foo', attrib.deployment.name
    assert_equal r, attrib.actual
    assert_equal 'requires', attrib.name
  end

  test "role is set on create" do
    t = Barclamp.find_by_name 'test'
    r = t.template.public_roles.first.name
    attrib = @role.require_deployment 'test', 'foo', r
    assert_equal 'test', attrib.deployment.barclamp.name
    assert_equal 'foo', attrib.deployment.name
    assert_equal r, attrib.actual
  end
end

