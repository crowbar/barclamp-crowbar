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

class BarclampsController < ApplicationController

  self.help_contents = Array.new(superclass.help_contents)

  def index
    @list = Barclamp.all
    respond_to do |format|
      format.html { }
      format.json { render api_index :barclamp, @list }
    end
  end

  def show
    respond_to do |format|
      format.html { @barclamp = Barclamp.find_key params[:id] }
      format.json { render api_show :barclamp, Barclamp }
    end
  end

  def update
    render api_not_supported("delete", "barclamp")
  end

  def destroy
    render api_not_supported 'delete', 'barclamp'
  end

  def create
    render api_not_supported 'post', 'barclamp'
  end
  
  # Redirects the requested to the snapshot that is the requested template
  def template
    redirect_to snapshot_path(:id=>barclamp.template_id)
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

end

