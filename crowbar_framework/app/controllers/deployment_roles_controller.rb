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

class DeploymentRolesController < ApplicationController

  def index
    @list = if params.has_key? :snapshot_id
              Snapshot.find_key(params[:snapshot_id]).deployment_roles
            elsif params.has_key? :role_id
              Role.find_key(params[:role_id]).deployment_roles
            else
              DeploymentRole.all
            end
                                  
    respond_to do |format|
      format.html { }
      format.json { render api_index :deployment_role, @list }
    end
  end

  def show
    respond_to do |format|
      format.html { @deployment_role = DeploymentRole.find_key params[:id] }
      format.json { render api_show :deployment_role, DeploymentRole }
    end
  end

  def create
    params[:role_id] = Role.find_key(params[:role]).id
    params[:snapshot_id] = Deployment.find_key(params[:deployment]).head.id
    @deployment_role = DeploymentRole.create! params
    respond_to do |format|
      format.html { render :action=>:show }
      format.json { render api_show :deployment_role, DeploymentRole, nil, nil, @deployment_role }
    end
  end

  def update
    unless Rails.env.development?
      render  api_not_supported("delete", "deployment_role")
    else
      render api_update :deployment_role, DeploymentRole
    end
  end

  def destroy
    unless Rails.env.development?
      render  api_not_supported("delete", "deployment_role")
    else
      render api_delete DeploymentRole
    end
  end  
  
end

