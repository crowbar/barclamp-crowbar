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
 
class ServiceObjectModelTest < ActiveSupport::TestCase

  def build_service_object(bcname)
    so = ServiceObject.new(Rails.logger)
    so.bc_name = bcname
    so
  end

  test "Random Password" do
    so = build_service_object("crowbar")
    password = so.random_password
    assert_equal password.length, 12
    password = so.random_password 16
    assert_equal password.length, 16
  end

  # XXX: Figure out how to test locking.

  test "Base Transition Function" do
    so = build_service_object("crowbar")
    answer = so.transition(nil,nil,nil)
    assert_equal answer, [200, ""]
  end


  test "Destroy Active Function - unknown prop" do
    so = build_service_object("crowbar")
    answer = so.destroy_active("unknown")
    assert_equal answer, [404, {}]
  end

  test "Destroy Active Function - not active prop" do
    so = build_service_object("crowbar")
    answer = so.destroy_active("unknown")
    assert_equal answer, [404, {}]
  end

end
