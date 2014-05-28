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

module Utils
  class BootstrapNavigation < SimpleNavigation::Renderer::Base
    def render(item_container)
      list_content = item_container.items.inject([]) do |list, item|
        li_options = item.html_options.reject do |k, v|
          k == :link
        end

        if include_sub_navigation?(item) and not item.sub_navigation.empty?
          html_options = item.html_options

          html_options[:link] ||= {}
          html_options[:link][:class] = "dropdown-toggle"
          html_options[:link]["data-toggle".to_sym] = "dropdown"

          item.html_options = html_options
        end

        li_content = tag_for(item)

        if include_sub_navigation?(item)
          item.sub_navigation.dom_class = "dropdown-menu"
          li_content << render_sub_navigation_for(item)
        end

        list << content_tag(
          :li,
          li_content,
          li_options
        )
      end.join

      if skip_if_empty? && item_container.empty?
        ""
      else
        content_tag(
          (options[:ordered] ? :ol : :ul),
          list_content,
          {
            :id => item_container.dom_id,
            :class => item_container.dom_class
          }
        )
      end
    end
  end
end
