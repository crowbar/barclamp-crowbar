# -*- encoding : utf-8 -*-
#
# Copyright 2014, SUSE Linux GmbH
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

class Chef
  class Recipe
    def search_env_filtered(type, query="*:*", sort='X_CHEF_id_CHEF_X asc',
                            start=0, rows=100, &block)
      # All cookbooks encode the barclamp name as the role name prefix, thus we can
      # simply grab it from the query (e.g. BC 'keystone' for role 'keystone-server'):
      barclamp = /^\w*:(\w*).*$/.match(query)[1]

      # There are two conventions to filter by barclamp proposal:
      #  1) Other barclamp cookbook: node[@cookbook_name][$OTHER_BC_NAME_instance]
      #  2) Same cookbook: node[@cookbook_name][:config][:environment]
      if barclamp == cookbook_name
        env = node[barclamp][:config][:environment]
      else
        env = "#{barclamp}-config-#{node[cookbook_name]["#{barclamp}_instance"]}"
      end
      filtered_query = "#{query} AND #{barclamp}_config_environment:#{env}"
      if block
        return search(type, filtered_query, sort, start, rows, &block)
      else
        return search(type, filtered_query, sort, start, rows)[0]
      end
    end

    def get_instance(query)
      results = search_env_filtered(:node, query)
      if results.length > 0
        instance = results[0]
        instance = node if instance.name == node.name
      else
        instance = node
      end
      instance
    end
  end
end
