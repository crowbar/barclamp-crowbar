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
#
class RolesController < ApplicationController

  def index
    @list = if params.include? :deployment_id
              Deployment.find_key(params[:deployment_id]).roles
            elsif params.include? :node_id
              Node.find_key(params[:node_id]).roles
            else
              Role.all
            end
    respond_to do |format|
      format.html { }
      format.json { render api_index :role, @list }
    end
  end

  def show
    respond_to do |format|
      format.html { @role = Role.find_key params[:id]  }
      format.json { render api_show :role, Role }
    end
  end

  def create
    if params.include? :deployment_id
      deployment = Deployment.find_key params[:deployment_id]
      role = Role.find_key params[:deployment][:role_id].to_i 
      role.add_to_snapshot deployment.head
      respond_to do |format|
        format.html { redirect_to snapshot_path(deployment.head.id) }
        format.json { render api_show :deployment, Deployment, nil, nil, deployment }
      end
    else
      params[:barclamp_id] = Barclamp.find_key(params[:barclamp]).id if params.include? :barclamp
      r = Role.create! params
      render api_show :role, Role, nil, nil, r 
    end
  end

  def update
    respond_to do |format|
      format.html { 
        # for HTML, we are processing form input from template overlays (similar to node_role update)
        # for nested JSON, this routine relies on the role overriding the update_template method
        @role = Role.find_key params[:id]
        if params.key? :dataprefix
          params[:data] ||= {}
          params.each do |k,v|
            if k.start_with? params[:dataprefix]
              key = k.sub(params[:dataprefix],"")
              @role.update_template(key, v)
            end
          end
        elsif params.key? :template
          @role.template = params[:template]
        end
        @role.save!
        render :action=>:show
      }
      format.json { render api_update :role, Role }
    end
  end

  # special function so API can set an single item in template
  def template
    role = Role.find_key params[:role_id]
    role.update_template params[:key], params[:value]
    role.save!
    render api_show :role, Role, nil, nil, role
  end

  def destroy
    unless Rails.env.development?
      render  api_not_supported("delete", "role")
    else
      render api_delete Role
    end
  end  

end
