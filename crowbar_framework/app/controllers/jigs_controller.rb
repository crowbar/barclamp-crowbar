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
class JigsController < ApplicationController

  def index
    respond_to do |format|
      format.html { @jigs = Jig.order("'order'") } # show.html.erb
      format.json { render api_index :jig, Jig.all }
    end
  end

  def show
    render api_show :jig, Jig
  end

  def create
    j = Jig.create params
    render api_show :jig, Jig, nil, nil, j
  end
  
  def destroy
    render api_delete Jig
  end
      
end
