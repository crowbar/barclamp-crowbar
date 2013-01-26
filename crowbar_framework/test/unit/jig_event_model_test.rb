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

class JigEventModelTest < ActiveSupport::TestCase

  test "Must have override object-missing" do
     JigEvent.create!(:name=>"foo", :type=>"JigEventFoo")
     e = assert_raise(ActiveRecord::SubclassNotFound) { JigEvent.find_by_name "foo" }
     assert_equal "The single-table inheritance mechanism failed...", e.message.truncate(48)
  end

  test "Must have override object - present" do
     c = JigEvent.create! :name=>"subclass", :type=>"JigEventTest"
     assert_equal false, c.nil?
     assert_equal c.name, "subclass"
     assert_equal c.type, "JigEventTest"
  end
  
  test "Returns correct objective type" do
    c = JigEvent.create! :name=>"type_test", :type=>"JigEventTest"
    t = JigEvent.find_by_name "type_test"
    assert_equal c.type, t.type
    assert_equal t.type, "JigEventTest"
    assert_kind_of JigEvent, c
  end
  
  
end

