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

module Haml
  module Compiler
    class << self
      def build_attributes_with_dasherize(is_html, attr_wrapper, escape_attrs, attributes = {})
        new_attributes = {}.tap do |dasherized|
          attributes.keys.each do |key|
            dasherized[key.to_s.gsub("_", "-").to_sym] = attributes[key]
          end
        end

        build_attributes_without_dasherize(
          is_html,
          attr_wrapper,
          escape_attrs,
          new_attributes
        )
      end

      alias_method :build_attributes_without_dasherize, :build_attributes
      alias_method :build_attributes, :build_attributes_with_dasherize
    end
  end
end

Haml::Template.options[:attr_wrapper] = "\""
