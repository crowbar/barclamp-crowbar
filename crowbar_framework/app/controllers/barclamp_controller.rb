# Copyright 2011, Dell 
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
# Author: RobHirschfeld 
# 
require 'chef'
require 'json'

class BarclampController < ApplicationController

  before_filter :controller_to_barclamp

  def controller_to_barclamp
    @bc_name = params[:barclamp] || params[:controller]
    @service_object.bc_name = @bc_name
  end

  self.help_contents = Array.new(superclass.help_contents)
  def initialize
    super()
    @service_object = ServiceObject.new logger
  end

  #
  # Barclamp List (generic)
  #
  # Provides the restful api call for
  # List Barclamps 	/crowbar 	GET 	Returns a json list of string names for barclamps 
  #
  add_help(:barclamp_index)
  def barclamp_index
    @barclamps = ServiceObject.all
    respond_to do |format|
      format.html { render :template => 'barclamp/barclamp_index' }
      format.xml  { render :xml => @barclamps }
      format.json { render :json => @barclamps }
    end
  end

  #
  # Provides the restful api call for
  # List Versions 	/crowbar/<barclamp-name> 	GET 	Returns a json list of string names for the versions 
  #
  add_help(:versions)
  def versions
    ret = @service_object.versions
    return render :text => ret[1], :status => ret[0] if ret[0] != 200
    render :json => ret[1]
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

    ret = @service_object.transition(id, name, state)
    return render :text => ret[1], :status => ret[0] if ret[0] != 200
    render :json => ret[1]
  end
  
  #
  # Provides the restful api call for
  # Show Instance 	/crowbar/<barclamp-name>/<version>/<barclamp-instance-name> 	GET 	Returns a json document describing the instance 
  #
  add_help(:show,[:id])
  def show
    ret = @service_object.show_active params[:id]
    @role = ret[1]
    Rails.logger.debug "Role #{ret.inspect}"
    respond_to do |format|
      format.html {
        return redirect_to proposal_barclamp_path :controller=>@bc_name, :id=>params[:id] if ret[0] != 200
        render :template => 'barclamp/show' 
      }
      format.xml  { 
        return render :text => @role, :status => ret[0] if ret[0] != 200
        render :xml => ServiceObject.role_to_proposal(@role, @bc_name)
      }
      format.json { 
        return render :text => @role, :status => ret[0] if ret[0] != 200
        render :json => ServiceObject.role_to_proposal(@role, @bc_name)
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
      ret = @service_object.destroy_active(params[:id])
      set_flash(ret, 'proposal.actions.delete_%s')
    rescue Exception => e
      Rails.logger.error "Failed to deactivate proposal: #{e.message}\n#{e.backtrace.join("\n")}"
      flash[:notice] = t('proposal.actions.delete_failure') + e.message
      ret = [500, flash[:notice] ]
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
    ret = @service_object.elements
    return render :text => ret[1], :status => ret[0] if ret[0] != 200
    render :json => ret[1]
  end

  #
  # Provides the restful api call for
  # List Nodes Available for Element 	/crowbar/<barclamp-name>/<version>/elements/<barclamp-instance-name> 	GET 	Returns a json list of nodes that can be assigned to that element 
  #
  add_help(:element_info,[:id])
  def element_info
    ret = @service_object.element_info
    return render :text => ret[1], :status => ret[0] if ret[0] != 200
    render :json => ret[1]
  end

  #
  # Provides the restful api call for
  # List Proposals 	/crowbar/<barclamp-name>/<version>/proposals 	GET 	Returns a json list of proposals for instances 
  #
  add_help(:proposals)
  def proposals
    ret = @service_object.proposals
    @proposals = ret[1]
    return render :text => @proposals, :status => ret[0] if ret[0] != 200
    respond_to do |format|
      format.html { 
        @proposals.map! { |p| ProposalObject.find_proposal(@bc_name, p) }
        render :template => 'barclamp/proposal_index' 
      }
      format.xml  { render :xml => @proposals }
      format.json { render :json => @proposals }
    end
  end
  
  #
  # Provides the restful api call for
  # List Instances 	/crowbar/<barclamp-name>/<version> 	GET 	Returns a json list of string names for the ids of instances 
  #
  add_help(:index)
  def index
    respond_to do |format|
      format.html { 
        @title ||= "#{@bc_name.titlecase} #{t('barclamp.index.members')}" 
        @count = -1
        members = {}
        list = Kernel.const_get("#{@bc_name.camelize}Service").method(:members).call
        cat = ServiceObject.barclamp_catalog
        i = 0
        list.each { |bc, order| members[bc] = { 'description' => cat['barclamps'][bc]['description'], 'order'=>order || 99999} if !cat['barclamps'][bc].nil? and cat['barclamps'][bc]['user_managed'] }
        @modules = get_proposals_from_barclamps(members).sort_by {|k,v| v[:order].to_i}
        render 'barclamp/index' 
      }
      format.xml  { 
        ret = @service_object.list_active
        @roles = ret[1]
        return render :text => @roles, :status => ret[0] if ret[0] != 200
        render :xml => @roles 
      }
      format.json { 
        ret = @service_object.list_active
        @roles = ret[1]
        return render :text => @roles, :status => ret[0] if ret[0] != 200
        render :json => @roles 
      }
    end
  end

  #
  # Currently, A UI ONLY METHOD
  #
  add_help(:modules)
  def modules
    @title = I18n.t('barclamp.modules.title')
    @count = 0
    barclamps = ServiceObject.barclamp_catalog['barclamps'].delete_if { |bc, props| !props['user_managed'] }
    @modules = get_proposals_from_barclamps(barclamps).sort 
    respond_to do |format|
      format.html { render 'index'}
      format.xml  { render :xml => @modules }
      format.json { render :json => @modules }
    end
  end

  #
  # Currently, A UI ONLY METHOD
  #
  def get_proposals_from_barclamps(barclamps)
    modules = {}
    active = RoleObject.active
    barclamps.each do |name, details|
      props = ProposalObject.find_proposals name
      modules[name] = { :description=>details['description'] || t('not_set'), :order=> details['order'], :proposals=>{}, :expand=>false, :members=>(details['members'].nil? ? 0 : details['members'].length) }
      begin
        modules[name][:allow_multiple_proposals] = Kernel.const_get("#{name.camelize}Service").method(:allow_multiple_proposals?).call
      rescue
        Rails.logger.debug "WARNING: could not resolve barclamp #{name}.  Please correct the naming to be the object name when camelized"
        modules[name][:allow_multiple_proposals] = false
        modules[name][:description] = "#{modules[name][:description]} !Dev Mode Note: Barlcamp does not have matching #{name.camelize}Service object.  You may want to set 'barclamp:\\user_managed: false' in the crowbar.yml file" if RAILS_ENV === 'development'
      end
      ProposalObject.find_proposals(name).each do |prop|        
        # active is ALWAYS true if there is a role and or status maybe true if the status is ready, unready, or pending.
        status = (["unready", "pending"].include?(prop.status) or active.include?("#{name}_#{prop.name}")) 
        @count += 1 unless @count<0  #allows caller to skip incrementing by initializing to -1
        modules[name][:proposals][prop.name] = {:id=>prop.id, :description=>prop.description, :status=>(status ? prop.status : "hold"), :active=>status}
        if prop.status === "failed"
          modules[name][:proposals][prop.name][:message] = prop.fail_reason 
          modules[name][:expand] = true
        end
      end        
    end
    modules
  end

  #
  # Provides the restful api call for
  # Show Proposal Instance 	/crowbar/<barclamp-name>/<version>/proposals/<barclamp-instance-name> 	GET 	Returns a json document for the specificed proposal 
  #
  add_help(:proposal_show,[:id])
  def proposal_show
    ret = @service_object.proposal_show params[:id]
    return render :text => ret[1], :status => ret[0] if ret[0] != 200
    @proposal = ret[1]
    @active = begin RoleObject.active(params[:controller], params[:id]).length>0 rescue false end
    flash[:notice] = @proposal.fail_reason if @proposal.failed?
    @attr_raw = params[:attr_raw] || false
    @dep_raw = params[:dep_raw] || false

    respond_to do |format|
      format.html { render :template => 'barclamp/proposal_show' }
      format.xml  { render :xml => @proposal.raw_data }
      format.json { render :json => @proposal.raw_data }
    end
  end

  #
  # Currently, A UI ONLY METHOD
  #
  add_help(:proposal_status,[:id, :barclamp, :name],[:get])
  def proposal_status
    proposals = {}
    i18n = {}
    begin
      active = RoleObject.active(params[:barclamp], params[:name])
      result = if params[:id].nil? 
        result = ProposalObject.all 
        result.delete_if { |v| v.id =~ /^#{ProposalObject::BC_PREFIX}/ }
      else
        [ProposalObject.find_proposal(params[:barclamp], params[:name])]
      end
      result.each do |prop|
        prop_id = "#{prop.barclamp}_#{prop.name}"
        status = (["unready", "pending"].include?(prop.status) or active.include?(prop_id))
        proposals[prop_id] = (status ? prop.status : "hold")
        i18n[prop_id] = {:proposal=>prop.name.humanize, :status=>t("proposal.status.#{proposals[prop_id]}", :default=>proposals[prop_id])}
      end
      render :inline => {:proposals=>proposals, :i18n=>i18n, :count=>proposals.length}.to_json, :cache => false
    rescue Exception=>e
      count = (e.class.to_s == "Errno::ECONNREFUSED" ? -2 : -1)
      Rails.logger.fatal("Failed to iterate over proposal list due to '#{e.message}'")
      # render :inline => {:proposals=>proposals, :count=>count, :error=>e.message}, :cache => false
    end
  end

  #
  # Provides the restful api call for
  # Create Proposal Instance 	/crowbar/<barclamp-name>/<version>/proposals 	PUT 	Putting a json document will create a proposal 
  #
  add_help(:proposal_create,[:name],[:put])
  def proposal_create
    Rails.logger.info "Proposal Create starting. Params #{params.inspect}"    
    controller = params[:controller]
    orig_id = params[:name] || params[:id]
    params[:id] = orig_id
    answer = [ 500, "Server issue" ]
    begin
      Rails.logger.info "asking for proposal of: #{params.inspect}"
      answer = @service_object.proposal_create params
      Rails.logger.info "proposal is: #{answer.inspect}"
      flash[:notice] =  answer[0] != 200 ? answer[1] : t('proposal.actions.create_success')
    rescue Exception => e
      flash[:notice] = e.message
      Rails.logger.debug e.backtrace.join("\n")
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
      ret = @service_object.proposal_edit params
      return render :text => ret[1], :status => ret[0] if ret[0] != 200
      return render :json => ret[1]
    else # This is UI.
      params[:id] = "bc-#{params[:barclamp]}-#{params[:name]}"
      if params[:submit] == t('barclamp.proposal_show.save_proposal')
        @proposal = ProposalObject.find_proposal_by_id(params[:id])

        begin
          @proposal["attributes"][params[:barclamp]] = JSON.parse(params[:proposal_attributes])
          @proposal["deployment"][params[:barclamp]] = JSON.parse(params[:proposal_deployment])
          @service_object.validate_proposal @proposal.raw_data
          @service_object.validate_proposal_elements @proposal.elements
          @proposal.save
          @service_object.validate_proposal_after_save @proposal.raw_data
          flash[:notice] = t('barclamp.proposal_show.save_proposal_success')
        rescue Exception => e
          flash[:notice] = e.message
        end
      elsif params[:submit] == t('barclamp.proposal_show.commit_proposal')
        @proposal = ProposalObject.find_proposal_by_id(params[:id])
 
        begin
          @proposal["attributes"][params[:barclamp]] = JSON.parse(params[:proposal_attributes])
          @proposal["deployment"][params[:barclamp]] = JSON.parse(params[:proposal_deployment])
          @service_object.validate_proposal @proposal.raw_data
          @service_object.validate_proposal_elements @proposal.elements
          @proposal.save
          @service_object.validate_proposal_after_save @proposal.raw_data

          answer = @service_object.proposal_commit(params[:name])
          flash[:notice] = answer[1] if answer[0] >= 300
          flash[:notice] = t('barclamp.proposal_show.commit_proposal_success') if answer[0] == 200
          if answer[0] == 202
            flash_msg = answer[1].map { |node_dns|
                 NodeObject.find_node_by_name(node_dns).alias
            }.join ", "
            flash[:notice] = "#{t('barclamp.proposal_show.commit_proposal_queued')}: #{flash_msg}"
          end
        rescue Exception => e
          flash[:notice] = e.message
        end
      elsif params[:submit] == t('barclamp.proposal_show.delete_proposal')
        begin
          answer = @service_object.proposal_delete(params[:name])
          set_flash(answer, 'barclamp.proposal_show.delete_proposal_%s')
        rescue Exception => e
          flash[:notice] = e.message
        end
        redirect_to barclamp_modules_path(:id=>(params[:barclamp] || ''))
        return
      elsif params[:submit] == t('barclamp.proposal_show.destroy_active')
        begin
          answer = @service_object.destroy_active(params[:name])
          set_flash(answer, 'barclamp.proposal_show.destroy_active_%s')
        rescue Exception => e
          flash[:notice] = e.message
        end
      elsif params[:submit] == t('barclamp.proposal_show.dequeue_proposal')
        begin
          answer = @service_object.dequeue_proposal(params[:name])
          set_flash(answer, 'barclamp.proposal_show.dequeue_proposal_%s')
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
    answer = @service_object.proposal_delete params[:id]
    set_flash(answer, 'proposal.actions.delete_%s')
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
    ret = @service_object.proposal_commit params[:id]
    return render :text => ret[1], :status => ret[0] if ret[0] >= 210
    render :json => ret[1], :status => ret[0]
  end

  #
  # Provides the restful api call for
  # Dequeue Proposal Instance 	/crowbar/<barclamp-name>/<version>/proposals/dequeue/<barclamp-instance-name> 	DELETE 	This action will dequeue an existing proposal.
  #
  add_help(:proposal_dequeue,[:id],[:delete])
  def proposal_dequeue
    answer = @service_object.dequeue_proposal params[:id]
    set_flash(answer, 'proposal.actions.dequeue_%s')
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

  private
  def set_flash(answer, common, success='success', failure='failure')
    if answer[0] == 200
      flash[:notice] = t(common % success)
    else
      flash[:notice] = t(common % failure)
      flash[:notice] += ": " + answer[1].to_s unless answer[1].to_s.empty?
    end
  end
end

