# Copyright 2013, Dell 
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

  self.help_contents = Array.new(superclass.help_contents)

  #
  # Barclamp List (generic)
  #
  # Provides the restful api call for
  # List Barclamps 	/crowbar 	GET 	Returns a json list of string names for barclamps 
  #
  add_help(:index)
  def index
    if params.has_key? :id
      bc = Barclamp.find_key params[:id]
      @barclamps = bc.members
      @title ||= "#{t 'barclamp.index.members'} #{t('barclamp.'+bc.name+'.index.title')}" 
    else
      @barclamps = Barclamp.order("order ASC")
      @title ||= t 'barclamp.index.title'
    end
    respond_to do |format|
      format.html { }
      format.json { render api_index :barclamp, @barclamps  }
    end
  end


  #
  # Barclamp catalog
  # 
  # Provides restful API call for 
  # List actions       /barclamp:/api_version:/catalog  GET 
  # 
  add_help(:catalog)
  def catalog     
    @bc = barclamp
    render :json => { :name=>"unknown"} unless @bc

    # TODO: find actions by introspection?
    render :json => {
      :name=>@bc.name, 
      :version=>@bc.version, 
      :api_version=>@bc.api_version,
      :api_version_accepts=>@bc.api_version_accepts, 
      :actions=>['node','group','jig', 'attrib'],
      :license=>@bc.license,
      :copyright=>@bc.copyright
    }
  end

  #
  # Provides the restful api call for
  # List Versions 	/crowbar/<barclamp-name> 	GET 	Returns a json list of string names for the versions 
  #
  add_help(:versions)
  def versions
    render :json => barclamp.versions
  end

  # Redirects the requested to the snapshot that is the requested template
  def template
    redirect_to snapshot_path(:id=>barclamp.template_id)
  end
  #
  # Provides the restful api call for
  # Transition 	/crowbar/<barclamp-name>/<version>/transition/<barclamp-snapshot-name> 	POST 	Informs the barclamp snapshot of a change of state in the specified node 
  # Transition 	/crowbar/<barclamp-name>/<version>/transition/<barclamp-snapshot-name>?state=<state>&name=<hostname> 	GET 	Informs the barclamp snapshot of a change of state in the specified node - The get is supported here to allow for the limited function environment of the installation system. 
  #
  # CB1
  add_help(:transition, [:id, :name, :state], [:get,:post])
  def transition
    id = params[:id]       # Provisioner id
    state = params[:state] # State of node transitioning
    name = params[:name] # Name of node transitioning

    ret = operations.transition(id, name, state)
    return render :text => ret[1], :status => ret[0] if ret[0] != 200
    render :json => Node.find_by_name(name).jig_hash.to_hash
  end

  # Quick visual for dev to see the Barclamp dependency graph
  add_help(:graph)
  def graph
    @barclamps = Barclamp.all
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
    end
  end

end

