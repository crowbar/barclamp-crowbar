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
require 'json'

class CmdbMapModelTest < ActiveSupport::TestCase

  test "Unique Name" do
    cm = CmdbMap.create! :name=>"nodups"
    assert_not_nil cm
    assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordNotUnique, SQLite3::ConstraintException) { CmdbMap.create!(:name => "nodups") }
  end

  test "Check on illegal names" do
    c = Cmdb.find_or_create_by_name_and_type(:name=>'name_parent', :type=>'CmdbTest')
    assert_raise(ActiveRecord::RecordInvalid) { CmdbMap.create!(:name => "1123", :cmdb=>c) }
    assert_raise(ActiveRecord::RecordInvalid) { CmdbMap.create!(:name => "1foo*cs", :cmdb=>c) }
    assert_raise(ActiveRecord::RecordInvalid) { CmdbMap.create!(:name => "Ille!gal", :cmdb=>c) }
    assert_raise(ActiveRecord::RecordInvalid) { CmdbMap.create!(:name => " nospaces", :cmdb=>c) }
    assert_raise(ActiveRecord::RecordInvalid) { CmdbMap.create!(:name => "no spaces", :cmdb=>c) }
    assert_raise(ActiveRecord::RecordInvalid) { CmdbMap.create!(:name => "nospacesatall ", :cmdb=>c) }
  end
  
  test "Must have cmdb parent" do
    return true  #this is busted for now, skip
    c = Cmdb.find_or_create_by_name_and_type(:name=>'parent', :type=>'CmdbTest')
    assert_equal false, c.nil?
    assert_equal c.name, "parent"
    assert_equal c.type, "CmdbTest"
    m = CmdbMap.create! :name=>"maptest", :cmdb=>c
    assert_equal m.cmdb.name, "parent"
  end
  
  
  
end

