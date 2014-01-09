# Copyright 2011-2013, Dell
# Copyright 2013, SUSE LINUX Products GmbH
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
# Author: Rob Hirschfeld
# Author: SUSE LINUX Products GmbH
#

require 'chef'

class MachinesController < ApplicationController

  self.help_contents = Array.new(superclass.help_contents)

  before_filter :set_name, :except => [:index, :list]

  def index
    if FileTest.exist? CHEF_CLIENT_KEY
      begin
        if session[:domain].nil? 
          session[:domain] = ChefObject.cloud_domain
        end
        @app = NodeObject.find_all_nodes
      rescue
        flash.now[:notice] = "ERROR: Could not connect to Chef Server at \"#{CHEF_SERVER_URL}.\""
        @app = []
      end
    else
      flash.now[:notice] = "ERROR: Could not find Chef Key at \"#{CHEF_CLIENT_KEY}.\""
    end

    respond_to do |format|
      format.json { render :json => @app.map! { |x| x.name } }
    end
  end

  add_help(:list)
  def list
    index
  end

  add_help(:show,[:name])
  def show
    if session[:domain].nil?
      session[:domain] = ChefObject.cloud_domain
    end

    machine = NodeObject.find_node_by_name @name
    if machine.nil?
      flash.now[:notice] = "ERROR: Could not find node for name #{@name}"
      respond_to do |format|
        format.json { render :text => "Host not found", :status => 404 }
      end
    else
      respond_to do |format|
        format.json { render :json => machine.to_hash }
      end
    end
  end

  add_help(:reinstall,[:name],[:post])
  def reinstall
    machine = NodeObject.find_node_by_name @name
    if machine.nil?
      flash.now[:notice] = "ERROR: Could not find node for name #{@name}"
      respond_to do |format|
        format.json { render :text => "Host not found", :status => 404 }
      end
    else
      machine.set_state("reinstall")
      respond_to do |format|
        format.json { render :json => {} }
      end
    end
  end

  add_help(:update,[:name],[:post])
  def update
    machine = NodeObject.find_node_by_name @name
    if machine.nil?
      flash.now[:notice] = "ERROR: Could not node for name #{@name}"
      respond_to do |format|
        format.json { render :text => "Host not found", :status => 404 }
      end
    else
      machine.set_state("update")
      respond_to do |format|
        format.json { render :json => {} }
      end
    end
  end

  add_help(:reset,[:name],[:post])
  def reset
    machine = NodeObject.find_node_by_name @name
    if machine.nil?
      flash.now[:notice] = "ERROR: Could not find node for name #{@name}"
      respond_to do |format|
        format.json { render :text => "Host not found", :status => 404 }
      end
    else
      machine.set_state("reset")
      respond_to do |format|
        format.json { render :json => {} }
      end
    end
  end

  add_help(:identify,[:name],[:post])
  def identify
    machine = NodeObject.find_node_by_name @name
    if machine.nil?
      flash.now[:notice] = "ERROR: Could not find node for name #{@name}"
      respond_to do |format|
        format.json { render :text => "Host not found", :status => 404 }
      end
    else
      machine.identify
      respond_to do |format|
        format.json { render :json => {} }
      end
    end
  end

  add_help(:shutdown,[:name],[:post])
  def shutdown
    machine = NodeObject.find_node_by_name @name
    if machine.nil?
      flash.now[:notice] = "ERROR: Could not find node for name #{@name}"
      respond_to do |format|
        format.json { render :text => "Host not found", :status => 404 }
      end
    else
      machine.shutdown
      respond_to do |format|
        format.json { render :json => {} }
      end
    end
  end

  add_help(:reboot,[:name],[:post])
  def reboot
    machine = NodeObject.find_node_by_name @name
    if machine.nil?
      flash.now[:notice] = "ERROR: Could not find node for name #{@name}"
      respond_to do |format|
        format.json { render :text => "Host not found", :status => 404 }
      end
    else
      machine.reboot
      respond_to do |format|
        format.json { render :json => {} }
      end
    end
  end

  add_help(:poweron,[:name],[:post])
  def poweron
    machine = NodeObject.find_node_by_name @name
    if machine.nil?
      flash.now[:notice] = "ERROR: Could not find node for name #{@name}"
      respond_to do |format|
        format.json { render :text => "Host not found", :status => 404 }
      end
    else
      machine.poweron
      respond_to do |format|
        format.json { render :json => {} }
      end
    end
  end

  add_help(:allocate,[:name],[:post])
  def allocate
    machine = NodeObject.find_node_by_name @name
    if machine.nil?
      flash.now[:notice] = "ERROR: Could not find node for name #{@name}"
      respond_to do |format|
        format.json { render :text => "Host not found", :status => 404 }
      end
    else
      machine.allocate
      respond_to do |format|
        format.json { render :json => {} }
      end
    end
  end

  add_help(:delete,[:name],[:delete])
  def delete
    machine = NodeObject.find_node_by_name @name
    if machine.nil?
      flash.now[:notice] = "ERROR: Could not find node for name #{@name}"
      respond_to do |format|
        format.json { render :text => "Host not found", :status => 404 }
      end
    else
      machine.set_state("delete")
      respond_to do |format|
        format.json { render :json => {} }
      end
    end
  end

  private

  # FIXME: the session[:domain] is only set for index action, while this is
  # called before every action. Is that a problem?
  def set_name
    @name = params[:name]
    @name = "#{@name}.#{session[:domain]}" if @name.split(".").length <= 1
  end
end
