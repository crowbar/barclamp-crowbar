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
class DeploymentController < ApplicationController

  def show
    @barclamp = Barclamp.find_key params[:barclamp]
    id = params[:id]
    @deployment = if id =~ /^[0-9]+$/
      Deployment.find id.to_i
    else
      Deployment.find_by_barclamp_id_and_name @barclamp.id, id
    end
  end
  
end
