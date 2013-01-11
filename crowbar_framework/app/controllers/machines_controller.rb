# Copyright 2011, Dell 
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
# Author: RobHirschfeld 
# 

class MachinesController < ApplicationController

  self.help_contents = Array.new(superclass.help_contents)

  def index
    if FileTest.exist? CHEF_CLIENT_KEY
      begin
        @app = Node.all
      rescue
        flash.now[:notice] = "ERROR: Could not connect to Chef Server at \"#{CHEF_SERVER_URL}.\""
        @app = []
      end
    else
      flash.now[:notice] = "ERROR: Could not find Chef Key at \"#{CHEF_CLIENT_KEY}.\""
    end

    respond_to do |format|
      format.html
      format.json { render :json => @app.map! { |x| x.name } }
    end
  end

  add_help(:list)
  def list
    index
  end

  add_help(:show,[:name])
  def show
    machine_ops { |machine|
      true
    }
  end

  add_help(:reinstall,[:name],[:post])
  def reinstall
    machine_ops { |machine|
      machine.set_state("reinstall")
    }
  end

  add_help(:update,[:name],[:post])
  def update
    machine_ops { |machine|
      machine.set_state("update")
    }
  end

  add_help(:reset,[:name],[:post])
  def reset
    machine_ops { |machine|
      machine.set_state("reset")
    }
  end

  add_help(:identify,[:name],[:post])
  def identify
    machine_ops { |machine|
      machine.identify
    }
  end

  add_help(:shutdown,[:name],[:post])
  def shutdown
    machine_ops { |machine|
      machine.shutdown
    }
  end

  add_help(:reboot,[:name],[:post])
  def reboot
    machine_ops { |machine|
      machine.reboot
    }
  end

  add_help(:poweron,[:name],[:post])
  def poweron
    machine_ops { |machine|
      machine.poweron
    }
  end

  add_help(:allocate,[:name],[:post])
  def allocate
    machine_ops { |machine|
      machine.allocate
    }
  end

  add_help(:delete,[:name],[:delete])
  def delete
    machine_ops { |machine|
      machine.delete
    }
  end

  private

  def machine_ops
    name = params[:name]
    name = "#{name}.#{session[:domain]}" unless session[:domain].nil? || name.include?(".")

    machine = Node.find_by_name name
    if machine.nil?
      flash.now[:notice] = "ERROR: Could not node for name #{name}"
      respond_to do |format|
        format.html
        format.json { render :text => "Host not found", :status => 404 }
      end
    else
      yield machine # execute the operation block passed in the method invocation
      respond_to do |format|
        format.html { redirect_to :action => :index }
        format.json { render :json => machine.jig_hash }
      end
    end
  end


end
