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
#
class CmdbController < ApplicationController

  # API GET /2.0/crowbar/2.0/cmdb
  def index
    @cmdbs = {}
    Cmdb.all.each { |c| @cmdbs[c.id]=c.name }
    respond_to do |format|
      format.html # index.html.haml
      format.json { render :json => @cmdbs }
    end
  end

  # API GET /2.0/crowbar/2.0/cmdb/1
  # API GET /2.0/crowbar/2.0/cmdb/chef
  def show
    @cmdb = Cmdb.find_key params[:id]
    if @cmdb
      respond_to do |format|
        format.html # show.html.erb
        format.json {
          render :json => @cmdb
        }
      end
    else
      render :text=>"Could not find cmdb '#{params[:id]}'", :status => 404
    end
  end

  # RESTful DELETE of the node resource
  def destroy
    target = Cmdb.find_key(params[:id])
    if target.nil?
      render :text=>"Could not find cmdb '#{params[:id]}'", :status => 404
    else
      if Node.delete(target.id) > 0
        render :text => "Node #{params[:id]} deleted!"
      else
        render :text=>"Could not delete cmdb '#{params[:id]}'", :status => 500
      end
    end
  end
  
  # RESTfule POST of the node resource
  def create
    if request.post?
      @cmdb = Cmdb.create! params
      render :json => @cmdb
    end
  end
  
end
