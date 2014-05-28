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
          :id => "proposal_deployment",
          :type => "hidden",
          :name => "proposal_deployment",
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
