
require 'test_helper'
 
class BarclampModelTest < ActiveSupport::TestCase

  def validate_deep_compare_prop_conf(conf, conf2)
    return if conf == nil and conf2 == nil
    assert conf != nil
    assert conf2 != nil
    assert_equal conf.status, conf2.status
  end

  def validate_deep_compare_prop(prop, prop2)
    return if prop == nil and prop2 == nil
    assert prop != nil
    assert prop2 != nil

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
    e = assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name => "crowbar") }
    assert_equal "Validation failed: Name Name item must be unique", e.message

    b = Barclamp.create(:name => "crowbar")
    b = b.save
    assert_equal false, b
  end

  test "Roles Relation" do
    b = Barclamp.find_by_name("crowbar")
    r = b.roles

    r1 = Role.find_by_name_and_barclamp_id("crowbar", b.id)

    assert_equal 1, r.size
    assert_equal true, r.include?(r1)
  end

  test "Template Relation" do
    b = Barclamp.find_by_name("ganglia")
    t = b.template

    p = Proposal.find_by_name_and_barclamp_id("template", b.id)
    assert_equal t, p
  end

  test "Proposals empty" do
    b = Barclamp.find_by_name("crowbar")
    t = b.proposals

    assert_equal [], t
  end

  test "Active Proposals empty" do
    b = Barclamp.find_by_name("crowbar")
    t = b.active_proposals

    assert_equal [], t
  end

  test "Operations function" do
    b = Barclamp.find_by_name("crowbar")
    assert_instance_of(CrowbarService, b.operations)
  end

  test "Versions" do
    b = Barclamp.find_by_name("crowbar")
    assert_equal [ "1.0" ], b.versions
  end

  test "Proposal Create" do
    b = Barclamp.find_by_name("crowbar")

    prop = b.create_proposal
    assert prop.name.starts_with?("create")
    validate_deep_compare_prop(prop, b.template)

    prop = b.create_proposal("fred")
    assert_equal "fred", prop.name
    validate_deep_compare_prop(prop, b.template)

    e = assert_raise(ActiveRecord::RecordInvalid) { prop = b.create_proposal("fred") }
    assert_equal "Validation failed: Name Name item must be unique", e.message
  end

  test "Proposal Get" do
    b = Barclamp.find_by_name("crowbar")
    assert_equal 1, b.proposals.size

    assert_equal nil, b.get_proposal("John")
    prop = b.get_proposal("fred")
    assert_instance_of(Proposal, prop)
    assert_equal "fred", prop.name
  end

  test "Proposal Delete" do
    b = Barclamp.find_by_name("crowbar")

    assert b.delete_proposal("fred")
    b.proposals.reload
    assert_equal nil, b.get_proposal("fred")

    e = assert_raise(ActiveRecord::RecordInvalid) { prop = b.create_proposal("fred") }
    assert_equal "Validation failed: Name Name item must be unique", e.message
  end

  test "Get Roles by Order" do
    b = Barclamp.find_by_name("crowbar")

    ro = b.get_roles_by_order
    assert 1, ro.length
    assert 1, ro[0].length
    assert 1, ro[0][0]name, "crowbar"
  end

  test "Import 1x"

end

