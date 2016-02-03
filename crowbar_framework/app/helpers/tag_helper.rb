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
            content.push value == nil ? "" : ": #{value}"
          end

          output.push content_tag(
            :li,
            content.join("\n")
          )
        end
      end
    )
  end

  # Directly generate a tag for the glyphicons web font icons
  def icon_tag(icon, text = nil, options = {})
    options[:class] = [options[:class], "glyphicon", "glyphicon-#{icon}"].compact.join(" ")
    [
      content_tag(:span, "", options),
      text
    ].flatten.join("\n").html_safe
  end

  def badge_tag(text, clazz = nil)
    content_tag(
      :div,
      text,
      :class => "badge #{clazz}".strip
    )
  end

  def progress_steps(current, min, max)
    width = if current == max
      100
    else
      100 / (max - min) * (current - min)
    end

    content_tag(
      :div,
      content_tag(
        :div,
        t(
          "progress.steps",
          curr: current,
          total: max
        ),
        style: "width: #{width.floor}%;",
        role: "progressbar",
        class: "progress-bar progress-bar-info progress-bar-striped",
        aria: {
          valuenow: current,
          valuemin: min,
          valuemax: max
        }
      ),
      class: "progress"
    )
  end 
end
