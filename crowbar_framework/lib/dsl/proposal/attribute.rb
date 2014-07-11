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

module Dsl
  module Proposal
    class Attribute
      attr_accessor :attrs
      attr_accessor :proposal
      attr_accessor :view
      attr_accessor :attribute

      def initialize(proposal, view)
        @attrs = proposal.raw_attributes
        @proposal = proposal
        @view = view

        concat(
          attributes_field
        )
      end

      def attribute
        if @attribute.is_a? Array
          @attribute
        else
          [@attribute.to_s]
        end
      end

      def attributes_field
        tag(
          :input,
          :id => "proposal_attributes",
          :type => "hidden",
          :name => "proposal_attributes",
          :value => attrs.to_json,
          "data-changed-state" => I18n.t("proposal.failures.unsaved_changes")
        )
      end

      def select_field(attribute, options = {})
        @attribute = attribute

        grouping(options.delete(:wrapper)) do
          collection = options.delete(:collection)

          selects = if collection.is_a? Symbol
            send(collection, attribute_value)
          else
            options_for_select(collection, attribute_value)
          end

          [
            labeling,
            select_tag(
              attribute_name,
              selects,

              defaults.merge({
                "data-change"         => changer("string"),
                "id"                  => sanitize_to_id(attribute_name),
                "data-initial-value"  => attribute_value
              }).merge(options)
            )
          ].join("\n")
        end
      end

      def boolean_field(attribute, options = {})
        @attribute = attribute

        grouping(options.delete(:wrapper)) do
          collection = options.delete(:collection)

          selects = if collection.is_a? Symbol
            send(collection, attribute_value)
          else
            if collection.nil?
              booleans_for_select(attribute_value)
            else
              options_for_select(collection, attribute_value)
            end
          end

          [
            labeling,
            select_tag(
              attribute_name,
              selects,

              defaults.merge({
                "data-change" => changer("boolean"),
                "id"          => sanitize_to_id(attribute_name),
              }).merge(options)
            )
          ].join("\n")
        end
      end

      def text_field(attribute, options = {})
        input attribute, :text_area_tag, "string", options
      end

      def string_field(attribute, options = {})
        input attribute, :text_field_tag, "string", options
      end

      def password_field(attribute, options = {})
        input attribute, :password_field_tag, "string", options
      end

      def float_field(attribute, options = {})
        input attribute, :text_field_tag, "float", options
      end

      def integer_field(attribute, options = {})
        input attribute, :number_field_tag, "integer", options
      end

      def array_string_field(attribute, options = {})
        input attribute, :text_field_tag, "array-string", options
      end

      def array_boolean_field(attribute, options = {})
        input attribute, :text_field_tag, "array-boolean", options
      end

      def array_integer_field(attribute, options = {})
        input attribute, :text_field_tag, "array-integer", options
      end

      def array_float_field(attribute, options = {})
        input attribute, :text_field_tag, "array-float", options
      end

      def instance_field(attribute, options = {})
        render_instance_selector(
          attribute.to_s,
          "#{attribute}_instance".to_sym,
          t(".#{attribute}_instance"),
          "#{attribute}_instance",
          proposal
        )
      end

      def header(dep_raw = false, attr_raw = true)
        content_tag(
          :h3,
          [
            t("barclamp.attributes"),
            content_tag(
              :div,
              content_tag(
                :small,
                proposal_raw_button(proposal, :dep_raw => dep_raw, :attr_raw => attr_raw)
              ),
              :class => "pull-right"
            )
          ].join("\n")
        )
      end

      def method_missing(method_name, *arguments, &block)
        if view.respond_to? method_name
          view.send(method_name, *arguments, &block)
        else
          super
        end
      end

      def attribute_name
        attribute.join("_")
      end

      def attribute_value
        begin
          # HACK: When this looks like a handlebars template, generate
          # a template lookup for handlebarsjs
          idx = attribute.index "{{@index}}"
          if idx
            return wrap_around(attribute.slice(idx+1, attribute.length).join("."), "{{", "}}")
          end

          result = attrs

          attribute.each do |n|
            result = result[n]
          end

          if result.is_a? Array
            result.join(", ")
          else
            result
          end
        rescue
          Rails.logger.debug "Failed to find the attribute for `#{attribute.join(", ")}`"
          ""
        end
      end

      protected

      def input(attribute, field, type_cast, options = {})
        @attribute = attribute

        grouping(options.delete(:wrapper)) do
          parameters = [
            attribute_name,
            attribute_value,

            defaults.merge({
              "data-change" => changer(type_cast),
              "id"          => sanitize_to_id(attribute_name),
            }).merge(options)
          ]

          [
            labeling,
            send(field, *parameters)
          ].join("\n")
        end
      end

      def grouping(options = {})
        content_tag(
          :div,
          yield,
          { :class => "form-group" }.merge(options || {})
        )
      end

      def defaults
        {
          :class => "form-control",
        }
      end

      def labeling
        translation_key = attribute.clone
        translation_key.unshift("")

        translation_key.map! do |v|
          v == "{{@index}}" ? "index" : v
        end

        content_tag(
          :label,
          t(translation_key.join(".")),
          :for => attribute
        )
      end

      def changer(type_cast)
        [
          attribute.join("/"),
          attribute_name,
          type_cast
        ].join(";")
      end

      # We need to make sure that FormTagHelpers sanitize_to_id does not mangle the
      # attribute. It can contain handlebars variable placeholders, which
      # would get escaped, e.g. {{@index}} would become __index__
      def sanitize_to_id(text)
        [*text].join("_")
      end
    end
  end
end
