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
 
class BarclampConfigModelTest < ActiveSupport::TestCase

  def setup
    @bc = Barclamp.create! :name=>'bc_config_test'
  end
  
  test "Unique per Barclamp Name" do
    b1 = Barclamp.create! :name=>"nodup1"
    b2 = Barclamp.create! :name=>"nodup2"
    assert_not_nil b1
    assert_not_nil b2
    bc1 = BarclampConfiguration.create :name=>"nodup", :barclamp_id=>b1.id
    assert_not_nil bc1
    bc2 = BarclampConfiguration.create :name=>"nodup", :barclamp_id=>b2.id
    assert_not_nil bc2
    assert_not_equal bc1.id, bc2.id
    
    e = assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordNotUnique, SQLite3::ConstraintException) { Barclamp.create!(:name => "nodup", :barclamp_id=>b1.id) }
  end
  
end

