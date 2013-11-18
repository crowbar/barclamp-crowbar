# Copyright 2011-2013, Dell
# Copyright 2013, SUSE LINUX Products GmbH
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
# Author: Dell Crowbar Team
# Author: SUSE LINUX Products GmbH
#

module FormHelper
  def platforms_for_select(selected)
    options_for_select(
      [
        [crowbar_service.pretty_target_platform(default_platform), default_platform],
        [crowbar_service.pretty_target_platform("windows-6.2"), "windows-6.2"],
        [crowbar_service.pretty_target_platform("hyperv-6.2"), "hyperv-6.2"]
      ],
      selected.to_s
    )
  end

  def booleans_for_select(selected)
    options_for_select(
      [["true", "true"], ["false", "false"]],
      selected.to_s
    )
  end

  def instance_selector_select(bc, name, field, proposal)
    service = eval("#{bc.camelize}Service.new nil")
    options = service.list_active[1] | service.proposals[1]

    if options.empty?
      options = [["None", ""]]
    else
      options = options.map do |x|
        [x.humanize,x]
      end
    end

    def_val = proposal.raw_attributes || ""

    field.split("/").each do |f|
      next if f.empty?
      break if def_val == ""
      def_val = def_val.send(f) || ""
    end

    select_tag(
      name,
      options_for_select(options, def_val),
      :class => "form-control",
      "data-change" => "#{field};#{name};string"
    )
  end

  def render_instance_selector(bc, name, label, field, proposal)
    return unless Kernel.const_get("#{bc.camelize}Service").method(:allow_multiple_proposals?).call

    render :partial => "barclamp/instance_selector", :locals => {
      :field => field,
      :label => label,
      :select_tag => instance_selector_select(bc, name, field, proposal)
    }
  end

  def number_field_tag(name, value = nil, options = {})
    options = options.stringify_keys
    options["type"] ||= "number"

    if range = options.delete("in") || options.delete("within")
      options.update("min" => range.min, "max" => range.max)
    end

    text_field_tag(name, value, options)
  end

  def defaults_for_select(options, item, scope = "")
    options_for_select(
      option_default(options, item, scope),
      item
    )
  end

  protected

  def option_default(options, item, scope = "")
    h = options.clone

    if h.nil?
      h = {
        t(item, :scope => scope) => item
      }
    else
      i = h.find{ |k, v| v == item }

      if i.nil?
        if item == ChefObject::NOT_SET or item.nil?
          h["[#{t(ChefObject::NOT_SET, :scope => scope)}]"] = item || ChefObject::NOT_SET
        else
          h["[#{item.humanize}]"] = item
        end
      else
        h.delete i[0]
        h["[#{i[0]}]"] = i[1] unless i[0].start_with?("[")
      end
    end

    h
  end
end
