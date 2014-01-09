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
  before_filter :load_machine_or_render_not_found, :except => [:index, :list]

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
    render_machine(@machine, :empty_response => false)
  end

  add_help(:reinstall,[:name],[:post])
  def reinstall
    @machine.set_state("reinstall")
    render_machine(@machine)
  end

  add_help(:update,[:name],[:post])
  def update
    @machine.set_state("update")
    render_machine(@machine)
  end

  add_help(:reset,[:name],[:post])
  def reset
    @machine.set_state("reset")
    render_machine(@machine)
  end

  add_help(:identify,[:name],[:post])
  def identify
    @machine.identify
    render_machine(@machine)
  end

  add_help(:shutdown,[:name],[:post])
  def shutdown
    @machine.shutdown
    render_machine(@machine)
  end

  add_help(:reboot,[:name],[:post])
  def reboot
    @machine.reboot
    render_machine(@machine)
  end

  add_help(:poweron,[:name],[:post])
  def poweron
    @machine.poweron
    render_machine(@machine)
  end

  add_help(:allocate,[:name],[:post])
  def allocate
    @machine.allocate
    render_machine(@machine)
  end

  add_help(:delete,[:name],[:delete])
  def delete
    @machine.set_state("delete")
    render_machine(@machine)
  end

  private

  # FIXME: the session[:domain] is only set for index action, while this is
  # called before every action. Is that a problem?
  def set_name
    @name = params[:name]
    @name = "#{@name}.#{session[:domain]}" if @name.split(".").length <= 1
  end

  def load_machine_or_render_not_found
    load_machine || render_not_found
  end

  def load_machine
    @machine = NodeObject.find_node_by_name @name
  end

  def render_not_found
    flash.now[:notice] = "ERROR: Could not find node for name #{@name}"
    respond_to do |format|
      format.json { render :text => "Host not found", :status => 404 }
    end
  end

  def render_machine(machine, options = {})
    options.reverse_merge!(:empty_response => true)
    respond_to do |format|
      format.json { render :json => options[:empty_response] ? {} : machine.to_hash }
    end
  end
end
