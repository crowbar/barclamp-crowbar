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

  # Barclamp List (generic)
  add_help(:barclamp_index)
  def barclamp_index
    throw "NOT IN USE - REMOVE >1.2"
    @barclamps = ServiceObject.all
    respond_to do |format|
      format.html { render :template => 'barclamp/barclamp_index' }
      format.xml  { render :xml => @barclamps }
      format.json { render :json => @barclamps }
    end
  end

  # Barclamp Show (specific one)
  add_help(:barclamp_show, [:id])
  def barclamp_show
    throw "NOT IN USE - REMOVE >1.2"
    @barclamp = ServiceObject.new logger
    @barclamp.bc_name = params[:id]

    respond_to do |format|
      format.html { render :template => 'barclamp/barclamp_show' }
      format.xml  { render :xml => @proposal }
      format.json
    end
  end

  # Barclamp Roles (all)
  add_help(:barclamp_roles, [])
  def barclamp_roles
    throw "DEPRICATED!"
    @roles = RoleObject.all
    @roles = @roles.delete_if { |v| v.nil? or !(v.name =~ /^.*-config-/) }
    respond_to do |format|
      format.html { render :template => 'barclamp/barclamp_roles' }
      format.xml  { render :xml => @roles }
      format.json { render :json => @roles }
    end
  end

  # REMOVE WHEN MENUS CAHNGES
  # Barclamp Proposals (all)
  add_help(:barclamp_proposals, [])
  def barclamp_proposals
    throw "DEPRICATED!"
    @proposals = ProposalObject.all
    @proposals = @proposals.delete_if { |v| v.nil? or v.id =~ /^#{ProposalObject::BC_PREFIX}/ }
    respond_to do |format|
      format.html { render :template => 'barclamp/barclamp_proposals' }
      format.xml  { render :xml => @proposals }
      format.json { render :json => @proposals }
    end
  end

  add_help(:versions)
  def versions
    ret = @service_object.versions
    return render :text => ret[1], :status => ret[0] if ret[0] != 200
    render :json => ret[1]
  end

  add_help(:transition, [:id, :name, :state], [:get,:post])
  def transition
    id = params[:id]       # Provisioner id
    state = params[:state] # State of node transitioning
    name = params[:name] # Name of node transitioning

    ret = @service_object.transition(id, name, state)
    return render :text => ret[1], :status => ret[0] if ret[0] != 200
    render :json => ret[1]
  end
  
  add_help(:index)
  def index
    @title = @bc_name.titlecase
    @members = Kernel.const_get("#{@bc_name.camelize}Service").method(:member_names).call
    respond_to do |format|
      format.html { 
        render :template => 'barclamp/index' 
      }
      format.xml  { 
        return render :text => @members, :status => ret[0] if ret[0] != 200
        render :xml => @members 
      }
      format.json {
        return render :text => @members, :status => ret[0] if ret[0] != 200
        render :json => @members 
      }
    end
  end
    
  def index_old
    ret = @service_object.list_active
    @roles = ret[1]
    
  end

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

  add_help(:delete,[:id],[:delete])
  def delete
    params[:id] = params[:id] || params[:name]
    ret = [500, "Server Problem"]
    begin
      ret = @service_object.destroy_active(params[:id])
      flash[:notice] = (ret[0] == 200 ? t('proposal.actions.delete_success') : t('proposal.actions.delete_fail') + ret[1])
    rescue Exception => e
      flash[:notice] = t('proposal.actions.delete_fail') + e.message
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

  # REMOVE, NOT USED IN NEW DESIGN
  add_help(:status,[],[:get])
  def status
    bcs = {}
    begin
      result = ProposalObject.all
      result = result.delete_if { |v| v.id =~ /^#{ProposalObject::BC_PREFIX}/ }
      result.each do |role|
        bcs[role.barclamp] = (bcs[role.barclamp].nil? ? 1 : bcs[role.barclamp] + 1)
      end
      render :inline => {:count=>bcs.length, :barclamps=>bcs}.to_json, :cache => false
    rescue Exception=>e
      count = (e.class.to_s == "Errno::ECONNREFUSED" ? -2 : -1)
      Rails.logger.fatal("Failed to iterate over proposals list due to '#{e.message}'")
    end
  end

  add_help(:elements)
  def elements
    ret = @service_object.elements
    return render :text => ret[1], :status => ret[0] if ret[0] != 200
    render :json => ret[1]
  end

  add_help(:element_info,[:id])
  def element_info
    ret = @service_object.element_info
    return render :text => ret[1], :status => ret[0] if ret[0] != 200
    render :json => ret[1]
  end

  add_help(:modules)
  def modules
    @title = I18n.t('barclamp.modules.title')
    @modules = {}
    @count = 0
    active = RoleObject.active nil
    list = ServiceObject.all
    list.each do |bc|
      name = bc[0]
      props = ProposalObject.find_proposals name
      @modules[name] = { :description=>bc[1], :proposals=>{}, :allow_multiple_proposals => (Kernel.const_get("#{name.camelize}Service").method(:allow_multiple_proposals?).call) }
      ProposalObject.find_proposals(bc[0]).each do |prop|        
        # active is ALWAYS true if there is a role and or status maybe true if the status is ready, unready, or pending.
        status = (["unready", "pending"].include?(prop.status) or active.include?("#{name}_#{prop.name}"))
        @count += 1
        @modules[name][:proposals][prop.name] = {:id=>prop.id, :description=>prop.description, :status=>(status ? prop.status : "hold"), :active=>status}
      end
    end
    respond_to do |format|
      format.html 
      format.xml  { render :xml => @modules }
      format.json { render :json => @modules }
    end
    
  end
    
  # REMOVE WHEN MENUS CHANGE!!!
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

  add_help(:proposal_show,[:id])
  def proposal_show
    ret = @service_object.proposal_show params[:id]
    return render :text => ret[1], :status => ret[0] if ret[0] != 200
    @proposal = ret[1]

    @attr_raw = params[:attr_raw] || false
    @dep_raw = params[:dep_raw] || false

    respond_to do |format|
      format.html { render :template => 'barclamp/proposal_show' }
      format.xml  { render :xml => @proposal.raw_data }
      format.json { render :json => @proposal.raw_data }
    end
  end

  add_help(:proposal_status,[],[:get])
  def proposal_status
    proposals = {}
    begin
      active = RoleObject.active params[:id]
      result = if params[:id].nil? 
        result = ProposalObject.all 
        result.delete_if { |v| v.id =~ /^#{ProposalObject::BC_PREFIX}/ }
      else
        [ProposalObject.find_proposal(params[:id][/^(.*)_(.*)$/,1], params[:id][/^(.*)_(.*)$/,2])]
      end
      result.each do |prop|
        prop_id = "#{prop.barclamp}_#{prop.name}"
        status = (["unready", "pending"].include?(prop.status) or active.include?(prop_id))
        proposals[prop_id] = (status ? prop.status : "hold")
      end
      render :inline => {:proposals=>proposals, :count=>proposals.length}.to_json, :cache => false
    rescue Exception=>e
      count = (e.class.to_s == "Errno::ECONNREFUSED" ? -2 : -1)
      Rails.logger.fatal("Failed to iterate over proposal list due to '#{e.message}'")
      # render :inline => {:proposals=>proposals, :count=>count, :error=>e.message}, :cache => false
    end
  end

  add_help(:proposal_create,[:name],[:put])
  def proposal_create
    Rails.logger.info "Proposal Create starting. Params #{params.to_s}"    
    controller = params[:controller]
    orig_id = params[:name] || params[:id]
    params[:id] = orig_id
    answer = [ 500, "Server issue" ]
    begin
      Rails.logger.info "asking for proposal of: #{params}"
      answer = @service_object.proposal_create params
      Rails.logger.info "proposal is: #{answer}"
      flash[:notice] =  answer[0] != 200 ? answer[1] : t('proposal.actions.create_success')
    rescue Exception => e
      flash[:notice] = e.message
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
          @proposal.save
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
          @proposal.save

          answer = @service_object.proposal_commit(params[:name])
          flash[:notice] = answer[1] if answer[0] >= 300
          flash[:notice] = t('barclamp.proposal_show.commit_proposal_success') if answer[0] == 200
          flash[:notice] = "#{t('barclamp.proposal_show.commit_proposal_queued')}: #{answer[1].inspect}" if answer[0] == 202
        rescue Exception => e
          flash[:notice] = e.message
        end
      elsif params[:submit] == t('barclamp.proposal_show.delete_proposal')
        begin
          answer = @service_object.proposal_delete(params[:name])
          flash[:notice] = answer[1] if answer[0] >= 300
          flash[:notice] = t('barclamp.proposal_show.delete_proposal_success') if answer[0] == 200
          return redirect_to barclamp_proposals_barclamp_path if answer[0] == 200
        rescue Exception => e
          flash[:notice] = e.message
        end
      elsif params[:submit] == t('barclamp.proposal_show.dequeue_proposal')
        begin
          answer = @service_object.dequeue_proposal(params[:name])
          flash[:notice] = t('barclamp.proposal_show.dequeue_proposal_failure') unless answer
          flash[:notice] = t('barclamp.proposal_show.dequeue_proposal_success') if answer
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

  add_help(:proposal_delete,[:id],[:delete])
  def proposal_delete
    answer = @service_object.proposal_delete params[:id]
    flash[:notice] = (answer[0] == 200 ? t('proposal.actions.delete_success') : t('proposal.actions.delete_fail'))
    respond_to do |format|
      format.html {         
        redirect_to barclamp_modules_path :id => @bc_name
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

  add_help(:proposal_commit,[:id],[:post])
  def proposal_commit
    ret = @service_object.proposal_commit params[:id]
    return render :text => ret[1], :status => ret[0] if ret[0] >= 210
    render :json => ret[1], :status => ret[0]
  end

  add_help(:proposal_dequeue,[:id],[:post])
  def proposal_dequeue
    ret = @service_object.dequeue_proposal params[:id]
    flash[:notice] = (ret[0]==200 ? t('proposal.actions.dequeue.success') : t('proposal.actions.dequeue.fail'))
    return render :text => flash[:notice], :status => 400 unless ret
    render :json => {}, :status => 200 if ret
  end

end

