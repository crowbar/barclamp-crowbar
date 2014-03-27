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

module ProposalsHelper
  def create_proposal_form_for(proposal, &block)
    url = create_proposal_path(
      controller: proposal
    )

    html = {
      method: :put,
      role: "form",
      class: "form-inline",
      autocomplete: "off",
      data: {
        type: "html",
        method: "put",
        blockui: t(".blockui_message")
      }
    }

    form_for(:proposal, url: url, html: html, &block).html_safe
  end

  def update_proposal_form_for(proposal, &block)
    url = update_proposal_path(
      controller: proposal.barclamp,
      id: proposal.name
    )

    html = {
      method: :post,
      role: "form",
      id: "update_proposal_form",
      autocomplete: "off",
      data: {
        type: "html",
        method: "post",
        blockui: t(".blockui_message")
      }
    }

    form_for(:proposal, url: url, html: html, &block).html_safe
  end

  def deactivate_proposal_button(proposal)
    link_to(
      t(".deactivate_proposal"),
      deactivate_proposal_path(controller: proposal.barclamp, id: proposal.name),
      class: "btn btn-default deactivate",
      data: {
        confirm: t(".deactivate_confirm")
      }
    ).html_safe
  end

  def delete_proposal_button(proposal)
    link_to(
      t(".delete_proposal"),
      delete_proposal_path(controller: proposal.barclamp, id: proposal.name),
      class: "btn btn-default delete",
      data: {
        confirm: t(".delete_confirm")
      }
    ).html_safe unless proposal["deployment"][proposal.barclamp]["crowbar-queued"]
  end

  def dequeue_proposal_button(proposal)
    link_to(
      t(".dequeue_proposal"),
      dequeue_proposal_path(controller: proposal.barclamp, id: proposal.name),
      class: "btn btn-default dequeue",
    ).html_safe if proposal["deployment"][proposal.barclamp]["crowbar-queued"]
  end

  def save_proposal_button(proposal)
    tag(
      :input,
      type: "submit",
      name: "submit",
      class: "btn btn-default save",
      value: t(".save_proposal"),
      data: {
        remote: true
      }
    ).html_safe
  end

  def apply_proposal_button(proposal)
    tag(
      :input,
      type: "submit",
      name: "apply",
      class: "btn btn-default apply",
      value: t(".apply_proposal"),
      data: {
        remote: true,
        confirm: t(".apply_confirm")
      }
    ).html_safe
  end

  def proposal_raw_button(proposal, options = {})
    parameters = {
      controller: proposal.barclamp,
      id: proposal.name,
    }.merge options

    link_to(
      t('raw'), 
      edit_proposal_path(parameters), 
      class: "rawview"
    ).html_safe
  end

  def proposal_custom_button(proposal, options = {})
    parameters = {
      controller: proposal.barclamp,
      id: proposal.name
    }.merge options

    link_to(
      t('custom'), 
      edit_proposal_path(parameters), 
      class: "customview"
    ).html_safe
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

  def link_to_proposal(barclamp)
    proposal = retrieve_proposal_for(barclamp)

    unless proposal.nil?
      link_to(
        t("proposal.actions.link", name: proposal.display_name),
        url_for_proposal(barclamp),
        class: "proposal #{barclamp}"
      ).html_safe
    else
      t("proposal.actions.link", name: display_name_for(barclamp))
    end
  end

  def url_for_proposal(barclamp)
    proposal = retrieve_proposal_for(barclamp)

    unless proposal.nil?
      show_proposal_path(
        controller: proposal.barclamp,
        id: proposal.name
      )
    else
      ""
    end
  end

  def link_to_proposal_with_name(barclamp, proposal)
    if proposal.nil?
      t("proposal.actions.link", name: display_name_for(barclamp))
    else
      link_to(
        t("proposal.actions.link", name: proposal),
        url_for_proposal_with_name(barclamp, proposal),
        class: "proposal #{barclamp}"
      ).html_safe
    end
  end

  def url_for_proposal_with_name(barclamp, proposal)
    if proposal.nil?
      ""
    else
      show_proposal_path(
        controller: barclamp,
        id: proposal
      )
    end
  end

  protected

  def retrieve_proposal_for(barclamp)
    @retrieve_proposal_for ||= {}

    @retrieve_proposal_for[barclamp] ||= begin
      proposals = ProposalObject.find_proposals(barclamp.to_s)

      unless proposals.empty? || proposals.length != 1
        proposals.first
      else
        nil
      end
    end
  end
end
