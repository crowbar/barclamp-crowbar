# Copyright 2012, Dell 
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
require 'json'

class BarclampController < ApplicationController

  def barclamp
   
    @bc_name = params[:barclamp] || params[:controller] unless @bc_name
    @barclamp_object = Barclamp.find_by_name(@bc_name) unless @barclamp_object
     puts "barclamp #{@bc_name} #{@barclamp_object}"
    @barclamp_object
  end
  private :barclamp

  def operations
    barclamp.operations(logger)
  end
  private :operations

  self.help_contents = Array.new(superclass.help_contents)

  #
  # Barclamp List (generic)
  #
  # Provides the restful api call for
  # List Barclamps 	/crowbar 	GET 	Returns a json list of string names for barclamps 
  #
  add_help(:barclamp_index)
  def barclamp_index
    @barclamps = Barclamp.all
    respond_to do |format|
      format.html { render :template => 'barclamp/barclamp_index' }
      format.json { render :json => @barclamps }
    end
  end

  #
  # Provides the restful api call for
  # List Versions 	/crowbar/<barclamp-name> 	GET 	Returns a json list of string names for the versions 
  #
  add_help(:versions)
  def versions
    render :json => barclamp.versions
  end

  #
  # Provides the restful api call for
  # Transition 	/crowbar/<barclamp-name>/<version>/transition/<barclamp-instance-name> 	POST 	Informs the barclamp instance of a change of state in the specified node 
  # Transition 	/crowbar/<barclamp-name>/<version>/transition/<barclamp-instance-name>?state=<state>&name=<hostname> 	GET 	Informs the barclamp instance of a change of state in the specified node - The get is supported here to allow for the limited function environment of the installation system. 
  #
  add_help(:transition, [:id, :name, :state], [:get,:post])
  def transition
    id = params[:id]       # Provisioner id
    state = params[:state] # State of node transitioning
    name = params[:name] # Name of node transitioning

    ret = operations.transition(id, name, state)
    return render :text => ret[1], :status => ret[0] if ret[0] != 200
    render :json => Node.find_by_name(name).jig_hash.to_hash
  end
  
  #
  # Provides the restful api call for
  # Show Instance 	/crowbar/<barclamp-name>/<version>/<barclamp-instance-name> 	GET 	Returns a json document describing the instance 
  #
  add_help(:show,[:id])
  def show
    prop = Proposal.find_by_name_and_barclamp_id(params[:id], barclamp.id)
    @role = prop.active_config if prop
    respond_to do |format|
      format.html {
        return redirect_to proposal_barclamp_path :controller=>@bc_name, :id=>params[:id] unless @role
        render :template => 'barclamp/show' 
      }
      format.json { 
        return render :text => t('proposal.failures.show_active_failed'), 
                      :status => 404 unless @role
        render :json => @role.to_proposal_object_hash
      }
    end
  end

  #
  # Provides the restful api call for
  # Destroy Instance 	/crowbar/<barclamp-name>/<version>/<barclamp-instance-name> 	DELETE 	Delete will deactivate and remove the instance 
  #
  add_help(:delete,[:id],[:delete])
  def delete
    params[:id] = params[:id] || params[:name]
    ret = [500, "Server Problem"]
    begin
      ret = operations.destroy_active(params[:id])
      flash[:notice] = (ret[0] == 200 ? t('proposal.actions.delete_success') : t('proposal.actions.delete_fail') + ret[1].to_s)
    rescue Exception => e
      flash[:notice] = t('proposal.actions.delete_fail') + e.message
      ret = [500, flash[:notice] + "\n" + e.backtrace.join("\n")]
    end

    respond_to do |format|
      format.html {
        redirect_to barclamp_modules_path(:id => @bc_name)
      }
      format.xml  { 
        return render :text => ret[1], :status => ret[0] if ret[0] != 200
        render :xml => {}
      }
      format.json { 
        return render :text => ret[1], :status => ret[0] if ret[0] != 200
        render :json => {}
      }
    end
  end

  #
  # Provides the restful api call for
  # List Elements 	/crowbar/<barclamp-name>/<version>/elements 	GET 	Returns a json list of roles that a node could be assigned to 
  #
  add_help(:elements)
  def elements
    render :json => barclamp.roles.map { |x| x.name }
  end

  #
  # Provides the restful api call for
  # List Nodes Available for Element 	/crowbar/<barclamp-name>/<version>/elements/<barclamp-instance-name> 	GET 	Returns a json list of nodes that can be assigned to that element 
  #
  add_help(:element_info,[:id])
  def element_info
    render :json => Node.all
  end

  #
  # Provides the restful api call for
  # List Proposals 	/crowbar/<barclamp-name>/<version>/proposals 	GET 	Returns a json list of proposals for instances 
  #
  add_help(:proposals)
  def proposals
    @proposals = barclamp.proposals
    @proposals_names = @proposals.map { |x| x.name }
    respond_to do |format|
      format.html { 
        render :template => 'barclamp/proposal_index'  # XXX: THIS DOESN"T EXIST
      }
      format.xml  { render :xml => @proposals_names }
      format.json { render :json => @proposals_names }
    end
  end
  
  # Quick visual for dev to see the Barclamp dependency graph
  add_help(:graph)
  def graph
    @barclamps = Barclamp.all
  end
  #
  # Provides the restful api call for
  # List Instances 	/crowbar/<barclamp-name>/<version> 	GET 	Returns a json list of string names for the ids of instances 
  #
  add_help(:index)
  def index
    respond_to do |format|
      format.html { 
        @title ||= "#{barclamp.display} #{t('barclamp.index.members')}" 
        @modules = barclamp.members
        render 'barclamp/index' 
      }
      format.xml  { 
        props = barclamp.active_proposals
        names = props.map { |x| x.name }
        render :xml => names
      }
      format.json { 
        props = barclamp.active_proposals
        names = props.map { |x| x.name }
        render :json => names
      }
    end
  end

  #
  # Currently, A UI ONLY METHOD
  #
  add_help(:modules)
  def modules
    @title = I18n.t('barclamp.modules.title')
    @barclamp = nil
    @modules = Barclamp.all.sort_by { |b| "%05d%s" % [b.order, b.name] }
    respond_to do |format|
      format.html { render 'index'}
      format.xml  { render :xml => @modules }
      format.json { render :json => @modules }
    end
  end

  #
  # Provides the restful api call for
  # Show Proposal Instance 	/crowbar/<barclamp-name>/<version>/proposals/<barclamp-instance-name> 	GET 	Returns a json document for the specificed proposal 
  #
  add_help(:proposal_show,[:id])
  def proposal_show
    @proposal = Proposal.find_by_name_and_barclamp_id(params[:id], barclamp.id)
    return render :text => t('proposal.failures.proposal_not_found'), :status => 404 unless @proposal
    @active = @proposal.active?
    flash[:notice] = @proposal.active_config.fail_reason if @active and @proposal.active_config.failed?
    @attr_raw = params[:attr_raw] || false
    @dep_raw = params[:dep_raw] || false

    respond_to do |format|
      format.html { render :template => 'barclamp/proposal_show' }
      format.xml  { render :xml => @proposal.current_config.to_proposal_object_hash }
      format.json { render :json => @proposal.current_config.to_proposal_object_hash }
    end
  end

  #
  # Provides the restful api call for
  # Create Proposal Instance 	/crowbar/<barclamp-name>/<version>/proposals 	PUT 	Putting a json document will create a proposal 
  #
  add_help(:proposal_create,[:name],[:put])
  def proposal_create
    Rails.logger.info "Proposal Create starting. Params #{params.to_s}"    
    controller = params[:controller]
    orig_id = params[:name] || params[:id]
    params[:id] = orig_id
    answer = [ 500, "Server issue" ]
    begin
      Rails.logger.info "asking for proposal of: #{params}"
      answer = operations.proposal_create params
      Rails.logger.info "proposal is: #{answer}"
      flash[:notice] =  answer[0] != 200 ? answer[1] : t('proposal.actions.create_success')
    rescue Exception => e
      flash[:notice] = e.message
      Rails.logger.error e.backtrace
    end
    respond_to do |format|
      format.html { 
        return redirect_to barclamp_modules_path :id => params[:controller] if answer[0] != 200
        redirect_to proposal_barclamp_path :controller=> controller, :id=>orig_id
      }
      format.xml  {
        return render :text => flash[:notice], :status => answer[0] if answer[0] != 200
        render :xml => answer[1] 
      }
      format.json {
        return render :text => flash[:notice], :status => answer[0] if answer[0] != 200
        render :json => answer[1] 
      }
    end
  end

  #
  # Provides the restful api call for
  # Edit Proposal Instance 	/crowbar/<barclamp-name>/<version>/propsosals/<barclamp-instance-name> 	POST 	Posting a json document will replace the current proposal 
  #
  add_help(:proposal_update,[:id],[:post])
  def proposal_update
    if params[:submit].nil?  # This is RESTFul path
      ret = operations.proposal_edit params
      return render :text => ret[1], :status => ret[0] if ret[0] != 200
      return render :json => ret[1]
    else # This is UI.
      params[:id] = params[:name]
      if params[:submit] == t('barclamp.proposal_show.save_proposal')
        begin
          tparams = { "attributes" => {},
                      "deployment" => {},
                      "id" => params[:name] } 
          tparams["attributes"][params[:barclamp]] = JSON.parse(params[:proposal_attributes])
          tparams["deployment"][params[:barclamp]] = JSON.parse(params[:proposal_deployment])
          ret = operations.proposal_edit tparams
          flash[:notice] = t('barclamp.proposal_show.save_proposal_success') if ret[0] == 200
          flash[:notice] = ret[1] if ret[0] != 200
        rescue Exception => e
          flash[:notice] = e.message
        end
      elsif params[:submit] == t('barclamp.proposal_show.commit_proposal')
        @proposal = ProposalObject.find_proposal_by_id(params[:id])
 
        begin
          tparams = { "attributes" => {},
                      "deployment" => {},
                      "id" => params[:name] } 
          tparams["attributes"][params[:barclamp]] = JSON.parse(params[:proposal_attributes])
          tparams["deployment"][params[:barclamp]] = JSON.parse(params[:proposal_deployment])
          ret = operations.proposal_edit tparams
          flash[:notice] = ret[1] if ret[0] != 200

          if ret[0] == 200
            answer = operations.proposal_commit(params[:name])
            flash[:notice] = answer[1] if answer[0] >= 300
            flash[:notice] = t('barclamp.proposal_show.commit_proposal_success') if answer[0] == 200
            if answer[0] == 202
              flash_msg = ""
              answer[1].each {|node_dns|
                  flash_msg << Node.find_by_name(node_dns).alias << ", "
              }
              flash[:notice] = "#{t('barclamp.proposal_show.commit_proposal_queued')}: #{flash_msg}"
            end
          end
        rescue Exception => e
          flash[:notice] = e.message
        end
      elsif params[:submit] == t('barclamp.proposal_show.delete_proposal')
        begin
          answer = operations.proposal_delete(params[:name])
          if answer[0] == 200
            flash[:notice] = t('barclamp.proposal_show.delete_proposal_success')
          else
            flash[:notice] = t('barclamp.proposal_show.delete_proposal_failure') + ": " + answer[1].to_s
          end
        rescue Exception => e
          flash[:notice] = e.message
        end
        redirect_to barclamp_modules_path(:id=>(params[:barclamp] || ''))
        return
      elsif params[:submit] == t('barclamp.proposal_show.destroy_active')
        begin
          answer = operations.destroy_active(params[:name])
          if answer[0] == 200 
            flash[:notice] = t('barclamp.proposal_show.destroy_active_success') 
          else
            flash[:notice] = t('barclamp.proposal_show.destroy_active_failure') + ": " + answer[1].to_s
          end
        rescue Exception => e
          flash[:notice] = e.message
        end
      elsif params[:submit] == t('barclamp.proposal_show.dequeue_proposal')
        begin
          answer = operations.dequeue_proposal(params[:name])
          if answer[0] == 200
            flash[:notice] = t('barclamp.proposal_show.dequeue_proposal_success')
          else
            flash[:notice] = t('barclamp.proposal_show.dequeue_proposal_failure') + ": " + answer[1].to_s
          end
        rescue Exception => e
          flash[:notice] = e.message
        end
      else
        Rails.logger.warn "Invalid action #{params[:submit]} for #{params[:id]}"
        flash[:notice] = "Invalid action #{params[:submit]}"
      end
      redirect_to proposal_barclamp_path(:controller => params[:barclamp], :id => params[:name]) 
    end
  end

  #
  # Provides the restful api call for
  # Destroy Proposal Instance 	/crowbar/<barclamp-name>/<version>/proposals/<barclamp-instance-name> 	DELETE 	Delete will remove a proposal 
  #
  add_help(:proposal_delete,[:id],[:delete])
  def proposal_delete
    answer = operations.proposal_delete params[:id]
    flash[:notice] = (answer[0] == 200 ? t('proposal.actions.delete_success') : t('proposal.actions.delete_fail'))
    respond_to do |format|
      format.html {         
        return render :text => flash[:notice], :status => answer[0] if answer[0] != 200
        render :text => answer[1]
      }
      format.xml  {
        return render :text => flash[:notice], :status => answer[0] if answer[0] != 200
        render :xml => answer[1] 
      }
      format.json {
        return render :text => flash[:notice], :status => answer[0] if answer[0] != 200
        render :json => answer[1] 
      }
    end
  end

  #
  # Provides the restful api call for
  # Commit Proposal Instance 	/crowbar/<barclamp-name>/<version>/proposals/commit/<barclamp-instance-name> 	POST 	This action will create a new instance based upon this proposal. If the instance already exists, it will be edited and replaced 
  #
  add_help(:proposal_commit,[:id],[:post])
  def proposal_commit
    ret = operations.proposal_commit params[:id]
    return render :text => ret[1], :status => ret[0] if ret[0] >= 210
    render :json => ret[1], :status => ret[0]
  end

  #
  # Provides the restful api call for
  # Dequeue Proposal Instance   /crowbar/<barclamp-name>/<version>/proposals/dequeue/<barclamp-instance-name>   DELETE   This action will dequeue an existing proposal.
  #
  add_help(:proposal_dequeue,[:id],[:delete])
  def proposal_dequeue
    answer = operations.dequeue_proposal params[:id]
    if answer[0] == 200
      flash[:notice] = t('proposal.actions.dequeue.success')
    else
      flash[:notice] = t('proposal.actions.dequeue.fail') + ": " + answer[1].to_s
    end
    if answer[0] == 200
      return render :json => {}, :status => answer[0]
    else
      return render :text => flash[:notice], :status => answer[0]
    end
  end

  add_help(:nodes,[],[:get]) 
  def nodes
    #Empty method to override if your barclamp has a "nodes" view.
  end

end

