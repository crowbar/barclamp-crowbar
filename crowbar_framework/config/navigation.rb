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

SimpleNavigation::Configuration.run do |navigation|
  navigation.renderer = SimpleNavigationRenderers::Bootstrap3
  navigation.consider_item_names_as_safe = true

  navigation.selected_class = "active"
  navigation.active_leaf_class = "leaf"

  navigation.items do |primary|
    primary.dom_class = "nav navbar-nav"
    primary.item :nodes, t("nav.nodes"), root_path do |secondary|
      secondary.item :dashboard, t("nav.dashboard"), dashboard_path
      secondary.item :list, t("nav.list"), nodes_list_path
      secondary.item :families, t("nav.families"), nodes_families_path
    end
    primary.item :barclamps, t("nav.barclamps"), barclamp_modules_path do |secondary|
      secondary.item :all_bc, t("nav.all_bc"), barclamp_modules_path
      secondary.item :crowbar_bc, t("nav.crowbar_bc"), index_barclamp_path(:controller=>'crowbar')
    end
    primary.item :utils, t("nav.utils"), utils_path do |secondary|
      secondary.item :util_logs, t("nav.util_logs"), utils_path
    end
  end
end
