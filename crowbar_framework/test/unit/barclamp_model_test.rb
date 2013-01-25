# Copyright 2012, Dell 
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
    r = Role.find_or_create_by_name :name=>'crowbar'
    b.roles << r unless b.roles.include? r
  end
  
  def validate_deep_compare_prop_conf(conf, conf2)
    return if conf == nil and conf2 == nil
    assert_not_nil conf
    assert_not_nil conf2
    assert_equal conf.status, conf2.status
  end

  def validate_deep_compare_prop(prop, prop2)
    return if prop == nil and prop2 == nil
    assert_not_nil prop
    assert_not_nil prop2

    assert_not_equal prop.id, prop2.id
    assert_not_equal prop.name, prop2.name
    assert_equal prop.last_applied_rev, prop2.last_applied_rev
    assert_equal prop.description, prop2.description

    validate_deep_compare_prop_conf(prop.active_config, prop2.active_config)
    validate_deep_compare_prop_conf(prop.current_config, prop2.current_config)

    # Make sure active and current are in the list of proposals.
    if prop.active_config
      assert prop.proposal_configs.map { |x| x.id }.include?(prop.active_config.id)
    end
    if prop2.active_config
      assert prop2.proposal_configs.map { |x| x.id }.include?(prop2.active_config.id)
    end
    if prop.current_config
      assert prop.proposal_configs.map { |x| x.id }.include?(prop.current_config.id)
    end
    if prop2.current_config
      assert prop2.proposal_configs.map { |x| x.id }.include?(prop2.current_config.id)
    end

    # Make sure that the proposals match as well.
    assert_equal prop.proposal_configs.size, prop2.proposal_configs.size
    (0..(prop.proposal_configs.size - 1)).each do |x|
      validate_deep_compare_prop_conf(prop.proposal_configs[x], prop2.proposal_configs[x])
    end
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

    r1 = Role.find_by_name_and_barclamp_id("crowbar", b.id)

    assert_equal 1, r.size
    assert_equal true, r.include?(r1)
  end

  test "Template Relation" do
    b = Barclamp.find_or_create_by_name(:name=>"crowbar")
    assert_not_nil b
    t = b.template

    p = Proposal.find_by_name_and_barclamp_id("template", b.id)
    assert_equal t, p
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

  test "Proposal Create" do
    b = Barclamp.find_or_create_by_name(:name=>"crowbar")
    assert_not_nil b
    begin 
      prop = b.create_proposal 
    rescue
      flunk("Exception on create proposal")
      return false
    end
    assert prop.name.starts_with?("create")
    validate_deep_compare_prop(prop, b.template)

    prop = b.create_proposal("fred")
    assert_equal "fred", prop.name
    validate_deep_compare_prop(prop, b.template)

    e = assert_raise(ActiveRecord::RecordInvalid) { prop = b.create_proposal("fred") }
  end

  test "Naming Conventions" do
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name=>"1123") }
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name=>"1foo") }
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name=>"Ille!gal") }
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name=>" nospaces") }
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name=>"no spaces") }
    assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name=>"nospacesatall ") }
  end
  
  test "Proposal Get" do
    b = Barclamp.find_or_create_by_name("crowbar")
    assert_not_nil b

    begin
      prop = b.create_proposal("fred")
    rescue
      flunk("Exception on create proposal")
      return false
    end

    assert_equal 1, b.proposals.size

    assert_equal nil, b.get_proposal("John")
    prop = b.get_proposal("fred")
    assert_instance_of(Proposal, prop)
    assert_equal "fred", prop.name
  end

  test "Proposal Delete" do
    b = Barclamp.find_or_create_by_name("crowbar")
    assert_not_nil b
    begin
      prop = b.create_proposal("fred")
    rescue
      flunk("Exception on create proposal")
      return false
    end
    b.reload

    assert b.delete_proposal(b.get_proposal("fred"))
    b.proposals.reload
    assert_equal nil, b.get_proposal("fred")

    prop = b.create_proposal("fred")
    b.proposals.reload
    assert_equal 1, b.proposals.size
  end

  test "Get Roles by Order" do
    b = Barclamp.find_or_create_by_name("crowbar")
    assert_not_nil b
    ro = b.roles
    assert_not_nil ro
    begin
      assert_equal 1, ro.length
      assert_equal 'crowbar', ro[0].name
    rescue
      flunk("Exception inside get roles due to missing roles by order")
    end
  end

  test "roles get priority from deployment section" do
    json = JSON::load File.open(File.join(TEST_DATA_PATH,"bc-foo.json"))    
    bc = Barclamp.new
    bc.name = "foo"
    Barclamp.import_1x_deployment(bc,json)
    rmap = {}
    bc.roles.all.each do |r|
      rmap [r.name] = r.priority
    end
    assert_equal 80, rmap['foo_mon_master']
    assert_equal 81, rmap['foo_mon']
    assert_equal 82, rmap['foo_store']
  end

  test "roles are ordered correctly" do
    json = JSON::load File.open(File.join(TEST_DATA_PATH,"bc-foo.json"))    
    bc = Barclamp.create :name=>"foo"
    Barclamp.import_1x_deployment(bc,json)
    ordered = bc.roles(true)    # (true) forces a reload of the model
    assert_equal 'foo_mon_master', ordered.first.name
    assert_equal 'foo_mon', ordered.second.name
    assert_equal 'foo_store', ordered.third.name
  end

  test "barclamp type from name works" do
    name = "test"
    # we need to make sure that the barclamp is not in the DB
    testclass = name.capitalize+"::"+("barclamp_"+name).camelize
    if File.exist? File.join('app', 'models', name, "barclamp_#{name}.rb")
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

