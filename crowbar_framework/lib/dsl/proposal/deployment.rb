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

module Dsl
  module Proposal
    class Deployment
      attr_accessor :attrs
      attr_accessor :proposal
      attr_accessor :view

      def initialize(proposal, view)
        @attrs = proposal.raw_deployment
        @proposal = proposal
        @view = view

        concat(
          deployments_field
        )
      end

      def deployments_field
        tag(
          :input,
          :id => "proposal_attributes",
          :type => "hidden",
          :name => "proposal_attributes",
          :value => attrs.to_json
        )
      end

      def header(dep_raw = true, attr_raw = false)
        content_tag(
          :h3,
          [
            t("barclamp.deployment"),
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
    end
  end
end
