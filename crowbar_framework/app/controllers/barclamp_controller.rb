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

class BarclampController < ApplicationController
  before_action :initialize_service
  before_action :controller_barclamp

  helper_method :current_barclamp

  #
  # List barclamps  /crowbar/modules/<version>  GET  List all available barclamps
  #
  add_help(:available_barclamps_url)
  def modules
    @title ||= I18n.t("barclamp.modules.title")
    @barclamps ||= available_barclamps

    barclamp_listing
  end

  #
  # List instances  /crowbar/<barclamp-name>/<version>  GET  List all grouped barclamps
  #
  add_help(:grouped_barclamps_url)
  def index
    @title ||= view_context.display_name_for(current_barclamp)
    @barclamps ||= selected_barclamps

    barclamp_listing
  end

  #
  # Barclamp status  /crowbar/<barclamp-name>/<version>/status  GET  Get status for all barclamps
  #
  add_help(:status_barclamp_url)
  def state
    @proposals = {}.tap do |proposals|
      active = RoleObject.active

      barclamps = case current_barclamp
      when "modules"
        available_barclamps
      else
        selected_barclamps
      end

      barclamps.each do |barclamp_name, barclamp|
        ProposalObject.find_proposals(
          barclamp_name
        ).each do |proposal|
          status = if active.include?(proposal.prop) or %w(unready pending).include?(proposal.status)
            proposal.status
          else
            "hold"
          end

          proposals[proposal.barclamp] ||= {}

          proposals[proposal.barclamp][proposal.name] = {
            name: proposal.name,
            title: proposal.name.humanize,
            state: status,
            status: I18n.t(status.to_s, scope: "proposal.status")
          }
        end
      end
    end

    respond_to do |format|
      format.xml { render xml: { proposals: @proposals } }
      format.json { render json: { proposals: @proposals } }
    end
  rescue StandardError => e
    log_exception(e)

    respond_to do |format|
      format.xml { render xml: { error: e.message }, status: 500 }
      format.json { render json: { error: e.message }, status: 500 }
    end
  end

  #
  # Transition  /crowbar/<barclamp-name>/<version>/transition/<barclamp-instance-name>  POST  Informs the barclamp instance of a change of state in the specified node 
  # Transition  /crowbar/<barclamp-name>/<version>/transition/<barclamp-instance-name>?name=<hostname>&state=<state>  GET  Informs the barclamp instance of a change of state in the specified node. 
  #
  add_help(:transition_barclamp_url, [:id, :name, :state], [:get, :post])
  def transition
    status, response = @service_object.transition(
      params[:id], 
      params[:name], 
      params[:state]
    )

    respond_to do |format|
      format.xml { render xml: response, status: status }
      format.json { render json: response, status: status }
    end
  end

  #
  # List proposals  /crowbar/<barclamp-name>/<version>/proposals  GET  Returns a json list of proposals for instances 
  #
  add_help(:proposals_url)
  def proposal_index
    status, response = @service_object.proposals

    case status
    when 200
      @proposals = response.map! do |p| 
        ProposalObject.find_proposal(current_barclamp, p)
      end

      respond_to do |format|
        format.html do
          render "barclamp/proposal_index"
        end

        format.xml { render xml: @proposals, status: status }
        format.json { render json: @proposals, status: status }
      end
    else
      respond_to do |format|
        format.html do
          redirect_to available_barclamps_url(selected: current_barclamp)
        end

        format.xml { render xml: { error: response }, status: status }
        format.json { render json: { error: response }, status: status }
      end
    end
  end

  #
  # Show proposal  /crowbar/<barclamp-name>/<version>/proposals/show/<barclamp-instance-name>  GET  Returns a json document describing the proposal 
  #
  add_help(:show_proposal_url, [:id], [:get])
  def proposal_show
    status, response = @service_object.show_active(
      params[:id]
    )

    case status
    when 200
      @role = response

      @proposal = ProposalObject.find_proposal(
        current_barclamp,
        params[:id]
      )

      respond_to do |format|
        format.html do
          render "barclamp/proposal_show"
        end

        format.xml { render xml: @proposal.raw_data, status: status }
        format.json { render json: @proposal.raw_data, status: status }
      end
    else
      respond_to do |format|
        format.html do
          flash[:alert] = I18n.t("barclamp.errors.proposal_find_failed")
          redirect_to proposals_path(controller: current_barclamp)
        end

        format.xml { render xml: { error: response }, status: status }
        format.json { render json: { error: response }, status: status }
      end
    end
  end

  #
  # Edit proposal  /crowbar/<barclamp-name>/<version>/proposals/<barclamp-instance-name>  GET  Returns a json document for the specificed proposal 
  #
  add_help(:edit_proposal_url, [:id], [:get])
  def proposal_edit
    status, response = @service_object.proposal_show(
      params[:id]
    )

    case status
    when 200
      @proposal = response
      @attr_raw = params[:attr_raw] == "true"
      @dep_raw = params[:dep_raw] == "true"

      @active = RoleObject.active(
        params[:controller], 
        params[:id]
      ).length > 0 rescue false

      respond_to do |format|
        format.html do
          flash.now[:alert] = @proposal.fail_reason if @proposal.failed?
          render "barclamp/proposal_edit"
        end

        format.xml { render xml: @proposal.raw_data, status: status }
        format.json { render json: @proposal.raw_data, status: status }
      end
    else
      respond_to do |format|
        format.html do
          flash[:alert] = I18n.t("barclamp.errors.proposal_find_failed")
          redirect_to proposals_path(controller: current_barclamp)
        end

        format.xml { render xml: { error: response }, status: status }
        format.json { render json: { error: response }, status: status }
      end
    end
  end

  #
  # Deactivate proposal  /crowbar/<barclamp-name>/<version>/proposals/deactivate/<barclamp-instance-name>  GET  Deactivate a proposal 
  #
  add_help(:deactivate_proposal_url, [:id], [:get])
  def proposal_deactivate
    status, response = @service_object.destroy_active(
      params[:id]
    )

    case status
    when 200
      respond_to do |format|
        format.html do
          flash[:success] = I18n.t("barclamp.errors.proposal_deactivated", barclamp: @service_object.display_name, proposal: params[:id])
          redirect_to available_barclamps_url(selected: current_barclamp)
        end

        format.xml { head :ok }
        format.json { head :ok }
      end
    else
      respond_to do |format|
        format.html do
          flash[:alert] = I18n.t("barclamp.errors.proposal_deactivate_failed", error: response)
          redirect_to edit_proposal_url(controller: current_barclamp, id: params[:id])
        end

        format.xml { render xml: { error: response }, status: status }
        format.json { render json: { error: response }, status: status }
      end
    end
  end

  #
  # Delete proposal  /crowbar/<barclamp-name>/<version>/proposals/<barclamp-instance-name>  DELETE  Delete a proposal
  #
  add_help(:delete_proposal_url, [:id], [:get])
  def proposal_delete
    status, response = @service_object.proposal_delete(
      params[:id]
    )

    case status
    when 200
      respond_to do |format|
        format.html do
          flash[:success] = I18n.t("barclamp.errors.proposal_deleted", barclamp: @service_object.display_name, proposal: params[:id])
          redirect_to available_barclamps_url(selected: current_barclamp)
        end

        format.xml { head :ok }
        format.json { head :ok }
      end
    else
      respond_to do |format|
        format.html do
          flash[:alert] = I18n.t("barclamp.errors.proposal_delete_failed", error: response)
          redirect_to edit_proposal_url(controller: current_barclamp, id: params[:id])
        end

        format.xml { render xml: { error: response }, status: status }
        format.json { render json: { error: response }, status: status }
      end
    end
  end

  #
  # Dequeue proposal  /crowbar/<barclamp-name>/<version>/proposals/dequeue/<barclamp-instance-name>   GET  This action will dequeue an existing proposal.
  #
  add_help(:dequeue_proposal_url, [:id], [:get])
  def proposal_dequeue
    status, response = @service_object.dequeue_proposal(
      params[:id]
    )

    case status
    when 200
      respond_to do |format|
        format.html do
          flash[:success] = I18n.t("barclamp.errors.proposal_dequeued", barclamp: @service_object.display_name, proposal: params[:id])
          redirect_to edit_proposal_url(controller: current_barclamp, id: params[:id])
        end

        format.xml { head :ok }
        format.json { head :ok }
      end
    else
      respond_to do |format|
        format.html do
          flash[:alert] = I18n.t("barclamp.errors.proposal_dequeue_failed", error: response)
          redirect_to edit_proposal_url(controller: current_barclamp, id: params[:id])
        end

        format.xml { render xml: { error: response }, status: status }
        format.json { render json: { error: response }, status: status }
      end
    end
  end

  #
  # Commit proposal  /crowbar/<barclamp-name>/<version>/proposals/commit/<barclamp-instance-name>  GET  Commit a proposal
  #
  add_help(:commit_proposal_url, [:id], [:get])
  def proposal_commit
    status, response = @service_object.proposal_commit(
      params[:id]
    )

    case status
    when 200
      respond_to do |format|
        format.html do
          flash[:success] = I18n.t("barclamp.errors.proposal_commited", barclamp: @service_object.display_name, proposal: params[:id])
          redirect_to edit_proposal_url(controller: current_barclamp, id: params[:id])
        end

        format.xml { head :ok }
        format.json { head :ok }
      end
    else
      respond_to do |format|
        format.html do
          flash[:alert] = I18n.t("barclamp.errors.proposal_commit_failed", error: response)
          redirect_to edit_proposal_url(controller: current_barclamp, id: params[:id])
        end

        format.xml { render xml: { error: response }, status: status }
        format.json { render json: { error: response }, status: status }
      end
    end
  end

  #
  # Create proposal  /crowbar/<barclamp-name>/<version>/proposals  PUT  Create proposal 
  #
  add_help(:create_proposal_url, [:name, :description], [:put])
  def proposal_create
    status, response = @service_object.proposal_create(
      {
        name: params[:name],
        description: params[:description]
      }.with_indifferent_access
    )

    case status
    when 200
      respond_to do |format|
        format.html do
          flash[:success] = I18n.t("barclamp.errors.proposal_created", barclamp: @service_object.display_name, proposal: params[:name])
          redirect_to edit_proposal_url(controller: current_barclamp, id: params[:name])
        end

        format.xml { render xml: { created: params[:name] }, status: status }
        format.json { render json: { created: params[:name] }, status: status }
      end
    else
      respond_to do |format|
        format.html do
          flash[:alert] = I18n.t("barclamp.errors.proposal_create_failed", error: response)
          redirect_to available_barclamps_url(selected: current_barclamp)
        end

        format.xml { render xml: { error: response }, status: status }
        format.json { render json: { error: response }, status: status }
      end
    end
  rescue StandardError => e
    log_exception(e)

    respond_to do |format|
      format.html do
        flash[:alert] = I18n.t("barclamp.errors.proposal_create_failed", error: e.message)
        redirect_to available_barclamps_url(selected: current_barclamp)
      end

      format.xml { render xml: { error: e.message }, status: 500 }
      format.json { render json: { error: e.message }, status: 500 }
    end
  end

  #
  # Edit proposal  /crowbar/<barclamp-name>/<version>/propsosals/<barclamp-instance-name>  POST  Update a proposal
  #
  add_help(:update_proposal_url, [:id], [:post])
  def proposal_update
    case
    when params[:submit]
      begin
        @proposal = ProposalObject.find_proposal_by_id("bc-#{params[:barclamp]}-#{params[:id]}")

        @proposal["attributes"][params[:barclamp]] = JSON.parse(params[:proposal_attributes])
        @proposal["deployment"][params[:barclamp]] = JSON.parse(params[:proposal_deployment])
        @service_object.save_proposal!(@proposal)

        respond_to do |format|
          format.html do
            flash[:success] = I18n.t("barclamp.errors.proposal_saved", barclamp: @service_object.display_name, proposal: params[:id])
            redirect_to edit_proposal_url(controller: params[:barclamp], id: params[:id], dep_raw: (params[:dep_raw] == "true" || nil), attr_raw: (params[:attr_raw] == "true" || nil))
          end

          format.xml { render xml: { updated: params[:id] }, status: status }
          format.json { render json: { updated: params[:id] }, status: status }
        end
      rescue StandardError => e
        log_exception(e)

        respond_to do |format|
          format.html do
            flash[:alert] = I18n.t("barclamp.errors.proposal_save_failed", error: e.message)
            # Maybe we can avoid this redirect in future versions and display the errors directly?
            redirect_to edit_proposal_url(controller: params[:barclamp], id: params[:id], dep_raw: (params[:dep_raw] == "true" || nil), attr_raw: (params[:attr_raw] == "true" || nil))
          end

          format.xml { render xml: { error: e.message }, status: 500 }
          format.json { render json: { error: e.message }, status: 500 }
        end
      end
    when params[:apply]
      begin
        @proposal = ProposalObject.find_proposal_by_id("bc-#{params[:barclamp]}-#{params[:id]}")

        @proposal["attributes"][params[:barclamp]] = JSON.parse(params[:proposal_attributes])
        @proposal["deployment"][params[:barclamp]] = JSON.parse(params[:proposal_deployment])
        @service_object.save_proposal!(@proposal)

        status, response = @service_object.proposal_commit(params[:id])

        case status
        when 202
          nodes = response.map do |node_dns|
            NodeObject.find_node_by_name(node_dns).alias
          end.join(", ")

          respond_to do |format|
            format.html do
              flash[:notice] = I18n.t("barclamp.errors.proposal_queued", barclamp: @service_object.display_name, proposal: params[:id], nodes: nodes)
              redirect_to edit_proposal_url(controller: params[:barclamp], id: params[:id], dep_raw: (params[:dep_raw] == "true" || nil), attr_raw: (params[:attr_raw] == "true" || nil)) 
            end

            format.xml { render json: { queued: nodes }, status: status }
            format.json { render json: { queued: nodes }, status: status }
          end
        when 200
          respond_to do |format|
            format.html do
              flash[:success] = I18n.t("barclamp.errors.proposal_applied", barclamp: @service_object.display_name, proposal: params[:id])
              redirect_to edit_proposal_url(controller: params[:barclamp], id: params[:id], dep_raw: (params[:dep_raw] == "true" || nil), attr_raw: (params[:attr_raw] == "true" || nil)) 
            end

          format.xml { render xml: { applied: params[:id] }, status: status }
          format.json { render json: { applied: params[:id] }, status: status }
          end
        else
          respond_to do |format|
            format.html do
              flash[:alert] = I18n.t("barclamp.errors.proposal_apply_failed", error: response)
              # Maybe we can avoid this redirect in future versions and display the errors directly?
              redirect_to edit_proposal_url(controller: params[:barclamp], id: params[:id], dep_raw: (params[:dep_raw] == "true" || nil), attr_raw: (params[:attr_raw] == "true" || nil))
            end

            format.xml { render xml: { error: response }, status: status }
            format.json { render json: { error: response }, status: status }
          end
        end
      rescue StandardError => e
        log_exception(e)

        respond_to do |format|
          format.html do
            flash[:alert] = I18n.t("barclamp.errors.proposal_apply_failed", error: e.message)
            # Maybe we can avoid this redirect in future versions and display the errors directly?
            redirect_to edit_proposal_url(controller: params[:barclamp], id: params[:id], dep_raw: (params[:dep_raw] == "true" || nil), attr_raw: (params[:attr_raw] == "true" || nil))
          end

          format.xml { render xml: { error: e.message }, status: 500 }
          format.json { render json: { error: e.message }, status: 500 }
        end
      end
    else
      status, response = @service_object.proposal_edit(
        params
      )

      case status
      when 200
        respond_to do |format|
          format.html do
            flash[:success] = I18n.t("barclamp.errors.proposal_updated", barclamp: @service_object.display_name, proposal: params[:id])
            redirect_to edit_proposal_url(controller: params[:barclamp], id: params[:id], dep_raw: (params[:dep_raw] == "true" || nil), attr_raw: (params[:attr_raw] == "true" || nil)) 
          end

          format.xml { render xml: { updated: params[:id] }, status: status }
          format.json { render json: { updated: params[:id] }, status: status }
        end
      else
        respond_to do |format|
          format.html do
            flash[:alert] = I18n.t("barclamp.errors.proposal_update_failed", error: response)
            # Maybe we can avoid this redirect in future versions and display the errors directly?
            redirect_to edit_proposal_url(controller: params[:barclamp], id: params[:id], dep_raw: (params[:dep_raw] == "true" || nil), attr_raw: (params[:attr_raw] == "true" || nil))
          end

          format.xml { render xml: { error: response }, status: status }
          format.json { render json: { error: response }, status: status }
        end
      end
    end
  end

  #
  # Proposal status  /crowbar/<barclamp-name>/<version>/proposals/status/<barclamp-instance-name>  GET  Status for proposal
  #
  add_help(:status_proposal_url, [:id], [:get])
  def proposal_status
    @proposals = {}.tap do |proposals|
      case params[:id]
      when current_barclamp
        active = RoleObject.active(params[:controller], "*")
        result = ProposalObject.find_proposals(params[:controller]).compact
      else
        active = RoleObject.active(params[:controller], params[:id])
        result = [ProposalObject.find_proposal(params[:controller], params[:id])].compact
      end

      result.each do |proposal|
        status = if active.include? proposal.prop or %w(unready pending).include? proposal.status
          proposal.status
        else
          "hold"
        end

        proposals[proposal.barclamp] ||= {}

        proposals[proposal.barclamp][proposal.name] = {
          name: proposal.name,
          title: proposal.name.humanize,
          state: status,
          status: I18n.t(status.to_s, scope: "proposal.status")
        }
      end
    end

    respond_to do |format|
      format.xml { render xml: { proposals: @proposals } }
      format.json { render json: { proposals: @proposals } }
    end
  rescue StandardError => e
    log_exception(e)

    respond_to do |format|
      format.xml { render xml: { error: e.message }, status: 500 }
      format.json { render json: { error: e.message }, status: 500 }
    end
  end

  #
  # List elements  /crowbar/<barclamp-name>/<version>/elements  GET  Returns a json list of roles that a node could be assigned to 
  #
  add_help(:elements_url)
  def element_index
    status, response = @service_object.elements

    respond_to do |format|
      format.html { render json: response, status: status }
      format.json { render json: response, status: status }
      format.xml { render xml: response, status: status }
    end
  end

  #
  # List nodes available for element  /crowbar/<barclamp-name>/<version>/elements/<barclamp-instance-name>  GET  Returns a json list of nodes that can be assigned to that element 
  #
  add_help(:show_element_url, [:id])
  def element_show
    status, response = @service_object.element_info(params[:id])

    respond_to do |format|
      format.html { render json: response, status: status }
      format.json { render json: response, status: status }
      format.xml { render xml: response, status: status }
    end
  end


  #
  # List versions  /crowbar/<barclamp-name>  GET  Returns a json list of strings for the versions 
  #
  add_help(:versions)
  def versions
    status, response = @service_object.versions

    respond_to do |format|
      format.html { render json: response, status: status }
      format.json { render json: response, status: status }
      format.xml { render xml: response, status: status }
    end
  end

  protected

  def initialize_service
    @service_object = ServiceObject.new logger
  end

  def controller_barclamp
    @bc_name = params[:barclamp] || params[:controller]
    @service_object.bc_name = @bc_name
  end

  def current_barclamp
    @bc_name
  end

  def available_barclamps
    @available_barclamps ||= begin
      member_barclamps = BarclampCatalog.barclamps

      member_barclamps.select do |name, props| 
        props["user_managed"]
      end
    end
  end

  def selected_barclamps
    @selected_barclamps ||= begin
      member_barclamps = BarclampCatalog.members(
        current_barclamp
      )

      available_barclamps.select do |name, props| 
        member_barclamps.has_key? name
      end
    end
  end

  def barclamp_listing
    @count = 0

    @modules = {}.tap do |modules|
      active = RoleObject.active

      @barclamps.each do |name, details|
        service = ServiceObject.get_service(name)

        members = details["members"].length rescue 0
        description = details["description"] || I18n.t("not_set")

        modules[name] = {
          members: members,
          description: description,
          order: details["order"],
          allow_multiple_proposals: service.allow_multiple_proposals?,
          suggested_proposal_name: service.suggested_proposal_name,
          expand: false,
          proposals: {}
        }

        ProposalObject.find_proposals(name).each do |proposal|
          status = if active.include? proposal.prop or %w(unready pending).include? proposal.status
            proposal.status
          else
            "hold"
          end

          modules[name][:proposals][proposal.name] = {
            id:  proposal.id, 
            description: proposal.description, 
            state: status,
            status: I18n.t(status.to_s, scope: "proposal.status")
          }

          if proposal.status == "failed"
            modules[name][:proposals][proposal.name][:message] = proposal.fail_reason 
            modules[name][:expand] = true
          end

          @count += 1
        end

        (1..20).each do |x|
          possible_name = "#{service.suggested_proposal_name}_#{x}"

          next if active.include? "#{name}_#{possible_name}"
          next if modules[name][:proposals].keys.include? possible_name

          modules[name][:suggested_proposal_name] = possible_name
          break
        end if service.allow_multiple_proposals?
      end
    end.sort_by { |k, v| "%05d%s" % [v[:order], k] }

    respond_to do |format|
      format.html { render "barclamp/index" }
      format.xml { render xml: @modules }
      format.json { render json: @modules }
    end
  end
end
