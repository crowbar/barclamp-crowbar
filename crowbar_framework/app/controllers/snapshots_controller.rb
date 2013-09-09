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
    @list = Snapshot.order("id DESC").all
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
          r = Role.find :first
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

  def anneal
    @snapshot = Snapshot.find_key params[:snapshot_id]
    # anneal requires PUT to run
    if request.put?
      # run anneal in the background (if stepping the skip when any nodes are in transistion)    
      if !params.include?(:step) or NodeRole.all_by_state(NodeRole::TRANSITION).length==0
        job1 = fork do
          %x[rails r NodeRole.anneal!] 
        end
        Process.detach(job1)
        flash[:notice] = I18n.t('layouts.snapshot.anneal.annealling')
      end
    end
    @list = NodeRole.peers_by_state(@snapshot, NodeRole::TRANSITION).order("cohort,id")

    respond_to do |format|
      format.html {  }
      format.json { render api_index :node_roles, @list }
    end
  end

  def graph
    @snapshot = Snapshot.find_key params[:snapshot_id]
    respond_to do |format|
      format.html {  }
      format.json { 
        graph = []
        @snapshot.node_roles.each do |nr|
          vertex = { "id"=> nr.id, "name"=> "#{nr.node.alias}: #{nr.role.name}", "data"=> {"$color"=>"#83548B"}, "$type"=>"square", "$dim"=>15, "adjacencies" =>[] }
          nr.children.each do |c|
            vertex["adjacencies"] << { "nodeTo"=> c.id, "nodeFrom"=> nr.id, "data"=> { "$color" => "#557EAA" } }
          end
          graph << vertex
        end
        render :json=>graph.to_json, :content_type=>cb_content_type(:list) 
      }
    end    
  end

  def propose
    snap = Snapshot.find_key params[:snapshot_id]
    new_snap = snap.propose params[:name]
    respond_to do |format|
      format.html { redirect_to snapshot_path(new_snap.id) }
      format.json { render api_show :snapshot, Snapshot, nil, nil, new_snap }
    end
  end

  def commit 
    snap = Snapshot.find_key params[:snapshot_id]
    snap.commit
    respond_to do |format|
      format.html { redirect_to snapshot_path(snap.id) }
      format.json { render api_show :snapshot, Snapshot, nil, nil, snap }
    end
  end

  def recall
    snap = Snapshot.find_key params[:snapshot_id]
    snap.recall
    respond_to do |format|
      format.html { redirect_to snapshot_path(snap.id) }
      format.json { render api_show :snapshot, Snapshot, nil, nil, snap }
    end      
  end

end
