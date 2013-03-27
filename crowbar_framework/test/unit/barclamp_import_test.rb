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
 
class BarclampImportTest < ActiveSupport::TestCase

  def setup
    # we need to make sure that we have crowbar role
    if Barclamp.find_by_name('crowbar').nil?
      Barclamp.import_1x 'crowbar'
    end
    b = Barclamp.find_by_name "crowbar"
    assert_not_nil b.template_id
    assert_equal "private", b.template.private_roles.first.name
    @error_class = (RUBY_VERSION == '1.8.7') ? NameError : ArgumentError
  end

  test "roles get priority from deployment section" do
    json = JSON::load File.open("test/data/foo/bc-template-foo.json")
    bc = Barclamp.import_1x "foo", nil, "test/data/foo"
    bc.import_template(json,"bc-foo.json")
    rmap = {}
    bc.template.roles.each do |r|
      rmap [r.name] = r.run_order
    end
    assert_equal 80, rmap['foo_mon_master']
    assert_equal 81, rmap['foo_mon']
    assert_equal 82, rmap['foo_store']
  end
  
  test "roles are ordered correctly" do
    bc = Barclamp.create :name=>"foo"
    bc.source_path = "test/data/foo"
    t = Snapshot.create(:name=>"test", :barclamp_id=>bc.id)
    bc.template_id = t.id
    bc.import_template
    bc.save
    ordered = bc.template.roles(true)    # (true) forces a reload of the model
    assert bc.template.roles(true).count>0
    assert_equal 'foo_mon_master', ordered.first.name
    assert_equal 'foo_mon', ordered.second.name
    assert_equal 'foo_store', ordered.third.name
  end

  test "barclamp import fails on missing yml" do
     e = assert_raise(RuntimeError) {  Barclamp.import_1x 'bar', nil, 'test/data/foo' }
     assert_equal "Barclamp name must match name from YML file", e.message
  end
  
  test "barclamp info set for import" do
    path = 'test/data/foo'
    bc = Barclamp.import_1x 'foo', nil, path
    assert_equal "Unit Test Barclamp Data", bc.description
    assert_equal "Foo", bc.display
    assert_equal 'https://crowbar.github.com/barclamp-foo', bc.online_help
    assert_equal 0, bc.version
    assert_equal path, bc.source_path
    assert_equal false, bc.allow_multiple_deployments
    assert_equal true, bc.user_managed
    # not sure why this is failing... assert_equal "unknown", bc.build_on
    assert_equal "unknown", bc.commit
  end

end

