
require 'test_helper'
 
class ProposalModelTest < ActiveSupport::TestCase

  test "Barclamp relation" do
    b = Barclamp.find_by_name("crowbar")
    p = Proposal.find_by_name_and_barclamp_id("template", b.id)
    assert_equal b, p.barclamp
  end

  test "Deep Clone" do
    b = Barclamp.find_by_name("crowbar")
    p = Proposal.find_by_name_and_barclamp_id("template", b.id)

    p_new = p.deep_clone
    assert_not_equal p.id, p_new.id
    assert_equal p.name, p_new.name
    assert_equal p.status, p_new.status
 
    assert_equal p.active_config, p_new.active_config
    assert_equal p.current_config, p_new.current_config
    assert_equal p.proposal_configs.size, p_new.proposal_configs.size
  end

end

