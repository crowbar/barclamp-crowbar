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

  navigation.items do |level1|
    level1.dom_class = "nav navbar-nav"

    level1.item :nodes, t("nav.nodes.title"), root_path do |level2|
      level2.item :dashboard, t("nav.nodes.dashboard"), dashboard_path
      level2.item :batch, t("nav.nodes.batch"), nodes_list_path
      level2.item :families, t("nav.nodes.families"), nodes_families_path, :if => proc { Rails.env.development? }
    end
    level1.item :barclamps, t("nav.barclamps.title"), barclamp_modules_path do |level2|
      level2.item :all, t("nav.barclamps.all"), barclamp_modules_path
      level2.item :crowbar, t("nav.barclamps.crowbar"), index_barclamp_path(:controller => "crowbar")
      level2.item :queue, t("nav.barclamps.queue"), deployment_queue_path
    end
    level1.item :utils, t("nav.utils.title"), utils_path do |level2|
      level2.item :logs, t("nav.utils.logs"), utils_path
      level2.item :logs, t("upgrade_title"), upgrade_path
    end
  end
end
