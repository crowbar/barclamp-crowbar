# -*- encoding : utf-8 -*-
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

module TagHelper
  # Convert a hash into a multidimensional unordered list
  def hash_to_ul(hash)
    if hash.empty?
      "&mdash;".html_safe
    else
      content_tag(
        :ul,
        [].tap do |output|
          hash.each do |key, value|
            content = []

            if key.is_a? Hash
              content.push hash_to_ul(key)
            else
              content.push content_tag(:em, key)
            end

            if value.is_a? Hash
              content.push hash_to_ul(value)
            else
              if value.is_a? Array
                content.push content_tag(
                  :ul,
                  value.map do |v|
                    content_tag(
                      :li,
                      v
                    )
                  end.join("\n").html_safe
                )
              else
                content.push value == nil ? "" : "#{content.pop}: #{value}"
              end
            end

            output.push content_tag(
              :li,
              content.map(&:strip).join("\n").html_safe
            )
          end
        end.join("\n").html_safe
      )
    end
  end

  # Directly generate a tag for the glyphicons web font icons
  def icon_tag(icon, text = nil, options = {})
    defaults = {
      class: "glyphicon glyphicon-#{icon}"
    }

    [
      content_tag(
        :span,
        "",
        defaults.merge(options)
      ),
      text
    ].flatten.join("\n").html_safe
  end

  def badge_tag(text, clazz = nil, options = {})
    defaults = {
      class: "badge #{clazz}".strip
    }
    
    content_tag(
      :div,
      text,
      defaults.merge(options)
    ).html_safe
  end
end
