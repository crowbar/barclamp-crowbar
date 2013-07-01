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
 
class DeploymentModelTest < ActiveSupport::TestCase

  def setup

  end
  
  test "Unique per Barclamp Name" do
    d = Deployment.create :name=>"nodup"
    assert_not_nil d
    e = assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordNotUnique, SQLite3::ConstraintException) { Deployment.create!(:name => "nodup") }
  end
  
  test "Check protections on illegal names" do
    assert_raise(ActiveRecord::RecordInvalid) { Deployment.create!(:name => "1123") }
    assert_raise(ActiveRecord::RecordInvalid) { Deployment.create!(:name => "1foo") }
    assert_raise(ActiveRecord::RecordInvalid) { Deployment.create!(:name => "Ille!gal") }
    assert_raise(ActiveRecord::RecordInvalid) { Deployment.create!(:name => " nospaces") }
    assert_raise(ActiveRecord::RecordInvalid) { Deployment.create!(:name => "no spaces") }
    assert_raise(ActiveRecord::RecordInvalid) { Deployment.create!(:name => "nospacesatall ") }
  end
  
end

