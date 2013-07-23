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
class SnapshotsController < ApplicationController
      
  def index
    @list = Snapshot.all
    respond_to do |format|
      format.html { }
      format.json { render api_index :snapshot, @list }
    end
  end

  def show
    respond_to do |format|
      format.html { 
        @snapshot = Snapshot.find_key params[:id] 
        @nodes = {}
        @roles = {}
        @node_roles = { }
        @snapshot.node_roles.each do |nr|
          n = nr.node
          r = nr.role
          @nodes[n.id] = n unless n.nil? or @nodes.has_key? n.id
          @roles[r.id] = r unless r.nil? or @roles.has_key? r.id
          @node_roles[n.id] ||= {}     unless n.nil? or r.nil?
          @node_roles[n.id][r.id] = nr unless n.nil? or r.nil?
        end
        # make sure we have at least 1 role
        if @roles.length == 0
          r = Role.find_key 'crowbar'
          @roles[r.id] = r
        end
        }
      format.json { render api_show :snapshot, Snapshot }
    end
  end

  def create
    unless Rails.env.development?
      render  api_not_supported("post", "snapshot")
    else
      r = Snapshot.create! params
      render api_show :snapshot, Snapshot, nil, nil, r 
    end
  end

  def update
    unless Rails.env.development?
      render  api_not_supported("delete", "snapshot")
    else
      render api_update :snapshot, Snapshot
    end
  end

  def destroy
    unless Rails.env.development?
      render  api_not_supported("delete", "snapshot")
    else
      render api_delete Snapshot
    end
  end  

  def transition
    @snapshot = Snapshot.find_key params[:snapshot_id]
    @list = @snapshot.transition
    respond_to do |format|
      format.html { render :template => 'node_roles/index' }
      format.json { render api_index :node_roles, @list }
    end
  end

  def cycle
    Jig.run()
    redirect_to snapshot_path(params[:snapshot_id])
  end

end
