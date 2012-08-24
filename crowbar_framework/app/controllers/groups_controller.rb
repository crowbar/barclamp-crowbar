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
class GroupsController < ApplicationController

  # GET /group/2.0/1
  # GET /group/2.0/group
  def show
    @group = Group.find_key params[:id]
    respond_to do |format|
      format.html # show.html.erb
      format.json { render :json => @group }
    end
  end

  # RESTful delete of the node
  def delete
    Group.delete Group.find_key(params[:id]).id
    render :text => "Group #{params[:id]} deleted!"
  end
  
  # RESTful create
  def new
    if request.post?
      @group = Group.create! params
      render :json => @group
    end
  end
  
end
