
require 'test_helper'
 
class BarclampModelTest < ActiveSupport::TestCase

  test "Get All descriptions" do
    descs = Barclamp.get_all_descriptions
    assert_equal Barclamp.all.size, descs.size
  end

  test "Unique Name" do
    e = assert_raise(ActiveRecord::RecordInvalid) { Barclamp.create!(:name => "crowbar") }
    assert_equal "Validation failed: Name Name item must be unique", e.message

    b = Barclamp.create(:name => "crowbar")
    b = b.save
    assert_equal false, b
  end

  test "Rolse Relation" do
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

end

