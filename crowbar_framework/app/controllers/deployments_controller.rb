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
class DeploymentsController < ApplicationController

  def index
    @list = Deployment.all
    respond_to do |format|
      format.html { }
      format.json { render api_index :deployment, @list }
    end
  end

  def show
    respond_to do |format|
      format.html {       } # show.html.erb
      format.json { render api_show :deployment, Deployment }
    end
  end

  def create
    respond_to do |format|
      format.html {  }
      format.json { render api_show :deployment, Deployment, nil, nil, deploy }
    end
  end

  def destroy
    render api_delete Deployment
  end
  
  def commit
    # TODO ZEHICLE
    deploy = Deployment.find_key params[:id]
    deploy.commit
    render api_show :deployment, Deployment, nil, nil, deploy
  end

  def recall
    # TODO ZEHICLE
    deploy = Deployment.find_key params[:id]
    deploy.recall
    render api_show :deployment, Deployment, nil, nil, deploy
  end

  def status
    # TODO ZEHICLE
    render :status=>501, :text=>I18n.t('work_in_progress', :message=>'Status Action: refactoring by CloudEdge')
  end

end
