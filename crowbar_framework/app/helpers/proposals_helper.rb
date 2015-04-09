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
    url = create_proposal_barclamp_path(
      :controller => proposal
    )

    html = {
      :method => :put,
      :role => "form",
      :class => "form-inline",
      :autocomplete => "off",
      "data-type" => "html",
      "data-blockui" => t(".blockui_message")
    }

    form_for :proposal, :url => url, :html => html, &block
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
      :class => "btn btn-default deactivate",
      :value => t(".destroy_active"),
      "data-confirm" => t(".destroy_changes")
    )
  end

  def delete_proposal_button(proposal)
    tag(
      :input,
      :type => "submit",
      :name => "submit",
      :class => "btn btn-default delete",
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
      :class => "btn btn-default dequeue",
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
      :class => "btn btn-default apply",
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
      :class => "btn btn-default save",
      :value => t(".save_proposal"),
      "data-remote" => true
    )
  end

  def proposal_raw_button(proposal, options = {})
    parameters = {
      :id => proposal.name,
      :controller => proposal.barclamp
    }.merge options

    parameters.delete(:attr_raw) if parameters[:attr_raw] == false
    parameters.delete(:dep_raw) if parameters[:dep_raw] == false

    link_to t('raw'), proposal_barclamp_path(parameters), :class => "rawview"
  end

  def proposal_custom_button(proposal, options = {})
    parameters = {
      :id => proposal.name,
      :controller => proposal.barclamp
    }.merge options

    parameters.delete(:attr_raw) if parameters[:attr_raw] == false
    parameters.delete(:dep_raw) if parameters[:dep_raw] == false

    link_to t('custom'), proposal_barclamp_path(parameters), :class => "customview"
  end

  def attributes_for(proposal, &block)
    attribute = Dsl::Proposal::Attribute.new(proposal, self)
    attribute.instance_eval(&block)

    ""
  end

  def deployment_for(proposal, &block)
    deployment = Dsl::Proposal::Deployment.new(proposal, self)
    deployment.instance_eval(&block)

    ""
  end

  def link_to_proposal(barclamp)
    proposal = retrieve_proposal_for(barclamp)

    unless proposal.nil?
      link_to(
        t("proposal.actions.link", :name => proposal.display_name),
        url_for_proposal(barclamp),
        :class => "proposal #{barclamp}"
      )
    else
      t("proposal.actions.link", :name => display_name_for(barclamp))
    end
  end

  def url_for_proposal(barclamp)
    proposal = retrieve_proposal_for(barclamp)

    unless proposal.nil?
      url_for(
        :controller => proposal.barclamp,
        :action => "proposal_show",
        :id => proposal.name
      )
    else
      ""
    end
  end

  def link_to_proposal_with_name(barclamp, proposal)
    if proposal.nil?
      t("proposal.actions.link", :name => display_name_for(barclamp))
    else
      link_to(
        t("proposal.actions.link", :name => proposal),
        url_for_proposal_with_name(barclamp, proposal),
        :class => "proposal #{barclamp}"
      )
    end
  end

  def url_for_proposal_with_name(barclamp, proposal)
    if proposal.nil?
      ""
    else
      url_for(
        :controller => barclamp,
        :action => "proposal_show",
        :id => proposal
      )
    end
  end

  protected

  def retrieve_proposal_for(barclamp)
    @retrieve_proposal_for ||= {}

    @retrieve_proposal_for[barclamp] ||= begin
      proposals = Proposal.where(barclamp: barclamp.to_s)

      unless proposals.empty? || proposals.length != 1
        proposals.first
      else
        nil
      end
    end
  end
end
