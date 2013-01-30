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
 
class ProposalModelTest < ActiveSupport::TestCase

  test "Barclamp relation" do
    b = Barclamp.find_by_name("crowbar")
    p = Proposal.find_by_name_and_barclamp_id("template", b.id)
    assert_equal b, p.barclamp
  end

  test "Deep Clone" do
    b = Barclamp.find_by_name("crowbar")
    p = Proposal.find_by_name_and_barclamp_id("template", b.id)

    p_new = p.deep_clone("newname")
    assert_not_equal p.id, p_new.id
    assert_equal "newname", p_new.name
    assert_equal p.status, p_new.status
 
    assert_equal p.active_config, p_new.active_config
    assert_equal p.current_config.config, p_new.current_config.config
    assert_equal p.current_config.status, p_new.current_config.status
    assert_equal p.current_config.failed_reason, p_new.current_config.failed_reason
    assert_equal p.proposal_configs.size, p_new.proposal_configs.size
  end
  
  test "Naming Conventions" do
    assert_raise(ActiveRecord::RecordInvalid) { Proposal.create!(:name=>"1123") }
    assert_raise(ActiveRecord::RecordInvalid) { Proposal.create!(:name=>"1foo") }
    assert_raise(ActiveRecord::RecordInvalid) { Proposal.create!(:name=>"Ille!gal")}
    assert_raise(ActiveRecord::RecordInvalid) { Proposal.create!(:name=>" nospaces") }
    assert_raise(ActiveRecord::RecordInvalid) { Proposal.create!(:name=>"no spaces") }
    assert_raise(ActiveRecord::RecordInvalid) { Proposal.create!(:name=>"nospacesatall ") }
  end

end

