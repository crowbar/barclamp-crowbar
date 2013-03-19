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
    deployments =
        if params[:barclamp_id]
          bc_id = (params[:barclamp_id].is_a? String) ?  Barclamp.find_real_key(params[:barclamp_id]) : params[:barclamp_id]
          Deployment.find_all_by_barclamp_id(bc_id)
        else
          Deployment.all
        end
    render api_index :deployment, deployments
  end

  def show
    respond_to do |format|
      format.html { 
                    @barclamp = Barclamp.find_key params[:barclamp_id]
                    id = params[:id]
                    @deployment = if id =~ /^[0-9]+$/
                      Deployment.find id.to_i
                    else
                      Deployment.find_by_barclamp_id_and_name @barclamp.id, id
                    end
      } # show.html.erb
      format.json { render api_show :deployment, Deployment }
    end
  end

  def create
    bc_id = (params[:barclamp_id] || params[:barclamp])
    bc = bc_id && Barclamp.find_key(bc_id)
    return render :status=>:not_acceptable, :text=>I18n.t('model.deployment.barclamp_context') unless bc

    if bc.allow_multiple_deployments or bc.deployments.count==0
      deploy = bc.create_proposal params
      respond_to do |format|
        format.html { redirect_to deployment_path(:barclamp_id=>bc.name, :id=>deploy.name) }
        format.json { render api_show :deployment, Deployment, nil, nil, deploy
        }
      end
    else
      render :status=>:not_acceptable, :text=>I18n.t('model.deployment.singleton')
    end
  end

  def update
    bcc = Deployment.find_key params[:id]
    raise "cannot change barclamp_id" unless params[:barclamp_id].nil? or params[:barclamp_id]==bcc.barclamp_id
    bcc.update_attribs params
  end
  
  def destroy
    render api_delete Deployment
  end
  
  def commit
    deploy = Deployment.find_key params[:id]
    deploy.commit
    render api_show :deployment, Deployment, nil, nil, deploy
  end

  def recall
    deploy = Deployment.find_key params[:id]
    deploy.recall
    render api_show :deployment, Deployment, nil, nil, deploy
  end

  def status
    render :status=>501, :text=>I18n.t('work_in_progress', :message=>'Status Action: refactoring by CloudEdge')
  end

end
