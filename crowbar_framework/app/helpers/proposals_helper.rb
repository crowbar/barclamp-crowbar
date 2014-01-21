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
# Author: Rob Hirschfeld
# Author: SUSE LINUX Products GmbH
#

module ProposalsHelper
  def create_proposal_form_for(proposal, &block)
    url = create_proposal_barclamp_path(
      :controller => proposal
    )

    html = {
      :method => :put,
      :role => "form",
      :id => "create_proposal_form",
      :class => "form-inline",
      :autocomplete => "off",
      "data-type" => "html",
      "data-method" => "put"
    }

    form_for :proposal, :url => url, :html => html, :remote => true, &block
  end

  def update_proposal_form_for(proposal, &block)
    url = update_proposal_barclamp_path(
      :id => proposal.name,
      :controller => proposal.barclamp
    )

    html = {
      :method => :post,
      :role => "form",
      :id => "update_proposal_form",
      :autocomplete => "off",
      "data-type" => "html",
      "data-blockui" => t(".blockui_message")
    }

    form_for :proposal, :url => url, :html => html, &block
  end

  def deactivate_proposal_button(proposal)
    tag(
      :input,
      :type => "submit",
      :name => "submit",
      :class => "btn btn-default",
      :value => t(".destroy_active"),
      "data-confirm" => t(".destroy_changes")
    )
  end

  def delete_proposal_button(proposal)
    tag(
      :input,
      :type => "submit",
      :name => "submit",
      :class => "btn btn-default",
      :value => t(".delete_proposal"),
      "data-confirm" => t(".delete_changes")
    ) unless proposal["deployment"][proposal.barclamp]["crowbar-queued"]
  end

  def dequeue_proposal_button(proposal)
    tag(
      :input,
      :type => "submit",
      :name => "submit",
      :source => "dequeue1",
      :match => "dequeue2",
      :class => "btn btn-default",
      :value => t(".dequeue_proposal"),
      "data-remote" => true
    ) if proposal["deployment"][proposal.barclamp]["crowbar-queued"]
  end

  def apply_proposal_button(proposal)
    tag(
      :input,
      :type => "submit",
      :name => "submit",
      :source => "commit1",
      :match => "commit2",
      :class => "btn btn-default",
      :value => t(".commit_proposal"),
      "data-confirm" => t(".apply_changes"),
      "data-remote" => true
    )
  end

  def save_proposal_button(proposal)
    tag(
      :input,
      :type => "submit",
      :name => "submit",
      :source => "save1",
      :match => "save2",
      :class => "btn btn-default",
      :value => t(".save_proposal"),
      "data-remote" => true
    )
  end

  def proposal_raw_button(proposal, options = {})
    parameters = {
      :id => proposal.name,
      :controller => proposal.barclamp
    }.merge options

    link_to t('raw'), proposal_barclamp_path(parameters)
  end

  def proposal_custom_button(proposal, options = {})
    parameters = {
      :id => proposal.name,
      :controller => proposal.barclamp
    }.merge options

    link_to t('custom'), proposal_barclamp_path(parameters)
  end

  def attributes_for(proposal, &block)
    attribute = Dsl::Proposal::Attribute.new(proposal, self)
    block.bind(attribute).call

    ""
  end

  def deployment_for(proposal, &block)
    deployment = Dsl::Proposal::Deployment.new(proposal, self)
    block.bind(deployment).call

    ""
  end
end
