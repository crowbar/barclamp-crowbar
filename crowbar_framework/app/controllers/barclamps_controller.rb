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

class BarclampsController < ApplicationController

  self.help_contents = Array.new(superclass.help_contents)

  def index
    render api_index :barclamp, Barclamp.all
  end

  def show
    render api_show :barclamp, Barclamp
  end

  def destroy
    render api_not_supported 'delete', 'barclamp'
  end
  
  def update
    render api_not_supported 'put', 'barclamp'
  end
  
  def create
    render api_not_supported 'post', 'barclamp'
  end
  
  # Redirects the requested to the snapshot that is the requested template
  def template
    redirect_to snapshot_path(:id=>barclamp.template_id)
  end

end

