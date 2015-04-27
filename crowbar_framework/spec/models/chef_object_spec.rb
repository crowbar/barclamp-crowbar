#
# Copyright 2011-2013, Dell
# Copyright 2013-2014, SUSE LINUX Products GmbH
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

require 'spec_helper'

describe ChefObject do
  let(:chef_object) { ChefObject }

  before do
    if chef_object
      chef_object.class_eval { class_variable_set(:@@CrowbarDomain, nil) }
    end
  end

  after do
    if chef_object
      chef_object.class_eval { class_variable_set(:@@CrowbarDomain, nil) }
    end
  end

  describe "query chef" do
    it "returns new query" do
      chef_object.query_chef.should be_a(Chef::Search::Query)
    end

    it "returns empty node on failure" do
      Chef::Search::Query.stubs(:new).raises(StandardError)
      chef_object.query_chef.should be_a(Chef::Node)
    end
  end
end
