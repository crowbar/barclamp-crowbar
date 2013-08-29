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
    if params.include? :deployment_id
      deployment = Deployment.find_key params[:deployment_id]
      @list = deployment.head.roles
    else
      @list = Role.all
    end
    respond_to do |format|
      format.html { }
      format.json { render api_index :role, @list }
    end
  end

  def show
    respond_to do |format|
      format.html { @role = Role.find_key params[:id] }
      format.json { render api_show :role, Role }
    end
  end

  def create
    if params.include? :deployment_id
      deployment = Deployment.find_key params[:deployment_id]
      role = Role.find_key params[:deployment][:roles].to_i 
      role.add_to_snapshot deployment.head
      respond_to do |format|
        format.html { redirect_to deployment_path(deployment.id) }
        format.json { render api_show :deployment, Deployment, nil, nil, deployment }
      end
    else
      unless Rails.env.development?
        render api_not_supported("post", "role")
      else
        r = Role.create! params
        render api_show :role, Role, nil, nil, r 
      end
    end
  end

  def update
    unless Rails.env.development?
      render  api_not_supported("put", "role")
    else
      render api_update :role, Role
    end
  end

  # special function so API can set an single item in template
  def template
    role = Role.find_key params[:role_id]
    role.update_template params[:key], params[:value]
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
