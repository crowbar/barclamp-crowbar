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

# REVIEW - IS THIS NEEDED?
class BarclampRolesController < ApplicationController

  def index
    render api_index :role, barclamp.roles.all
  end

  def show
    render api_show :role, Role
  end
  
  def attribs
    id = params[:id]
    ri = Role.find_key id
    if ri.nil?
      render :text=>I18n.t('api.not_found', :id=>id, :type=>'role'), :status => 404
    elsif request.get?
      out = {:list=> [], :type=>:role, :link=>attribs_path(:id=>id)}
      ri.attribs.all.each { |ai| out[:list] << ai }
      render :json=>out
    else
      render :text=>I18n.t('api.not_supported', :verb=>'PUT/POST/DELETE', :obj=>'role'), :status => 501
    end
  end

  def nodes
    render :status=>501, :text=>I18n.t('work_in_progress', :message=>'Nodes List: refactoring by CloudEdge')
  end
  
end
