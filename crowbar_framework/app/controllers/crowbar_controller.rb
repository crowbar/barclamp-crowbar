# Copyright 2012, Dell 
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

class CrowbarController < BarclampController
  
  def index
    @title = I18n.t('title', :scope=>'barclamp.crowbar.index')
    super
  end

  def catalog
    render :json => {
      :name=>'crowbar', 
      :version=>'2.0', 
      :accepts=>['2.0'], 
      :actions=>['node','group','jig', 'attrib'],
      :license=>'apache2', 
      :copyright=>'Dell, Inc 2012'
    }
  end
  
  def node
    unless params[:version].eql?('2.0')
      render :text=>I18n.t('api.wrong_version', :version=>params[:version]) 
      return
    end
    @node = Node.find_key(params[:id]) if params[:id]
    if params[:target]
      node_attribs
    end
  end
  
  def node_attribs
    unless params[:version].eql?('2.0')
      render :text=>I18n.t('api.wrong_version', :version=>params[:version]) 
      return
    end
    @node = Node.find_key(params[:id]) if params[:id]
    @attrib = Attrib.find_key(params[:target_id]) if params[:target_id]
    # POST and PUT (do the same thing since PUT will create the missing info)
    if request.post? or request.put?
      if @node.nil?
         render :text=>I18n.t('api.not_found', :type=>'node', :id=>params[:id]), :status => 404
      else
        @na = @node.attrib_get(params[:target_id])
        if params["value"]
          @na.actual = params["value"]
          @na.save
        end
        render :json => @na
      end
    # DELETE
    elsif request.delete? and @attrib
      id = @node.attrib_get(@attrib)
      id.delete
      render :text=>I18n.t('api.deleted', :id=>id.object_id, :obj=>'attrib_instance')
    # fall through REST actions (all require ID)
    elsif request.get? and @attrib
      @na = AttribInstance.find_by_node_id_and_attrib_id @node.id, @attrib.id
      render :json => @na
    elsif params[:target_id]
      render :text=>I18n.t('api.not_found', :type=>'attrib_instance', :id=>params[:target_id]), :status => 404
    # list (no ID)
    elsif request.get?  
      attribs = {}
      @node.attrib_instances.each { |a| attribs[a.attrib.id] = (a.value.nil? ? 'null' : a.value ) }
      render :json => attribs
    # Catch
    else
      render :text=>I18n.t('api.unknown_request'), :status => 400
    end
    
  end
  
  def jig
    unless params[:version].eql?('2.0')
      render :text=>I18n.t('api.wrong_version', :version=>params[:version]) 
      return
    end
    @jig = Jig.find_key(params[:id]) if params[:id]
    
    # POST
    if request.post?
      @jig = Jig.create params
      render :json => @jig
    # PUT (not supported)
    elsif request.put?
      render :text=>I18n.t('api.not_supported', :action=>'PUT', :obj=>'jig'), :status => 504
    # DELETE
    elsif request.delete? and @jig
      Jig.delete @jig.id
      render :text=>I18n.t('api.deleted', :id=>@jig.id, :obj=>'jig')
    # fall through REST actions (all require ID)
    elsif request.get? and @jig
      render :json => @jig
    elsif params[:id]
      render :text=>I18n.t('api.not_found', :type=>'jig', :id=>params[:id]), :status => 404
    # list (no ID)
    elsif request.get?  
      jigs = {}
      Jig.all.each { |c| jigs[c.id] = c.name }
      render :json => jigs
    # Catch
    else
      render :text=>I18n.t('api.unknown_request'), :status => 400
    end
  end


  def barclamp_temp
    # TODO: temp method name until we figure out routing. see routes.rb
    unless params[:version].eql?('2.0')
      render :text=>I18n.t('api.wrong_version', :version=>params[:version]) 
      return
    end
    @barclamp = Barclamp.find_key(params[:id]) if params[:id]
    
    # POST
    if request.post?
      render :text=>I18n.t('api.not_supported', :verb=>'POST', :obj=>'barclamp'), :status => 405
    # PUT (not supported)
    elsif request.put?
      render :text=>I18n.t('api.not_supported', :verb=>'PUT', :obj=>'barclamp'), :status => 405
    # DELETE
    elsif request.delete?
      render :text=>I18n.t('api.not_supported', :verb=>'DELETE', :obj=>'barclamp'), :status => 405
    # fall through REST actions (all require ID)
    elsif request.get? and @barclamp
      render :json => @barclamp
    elsif params[:id]
      render :text=>I18n.t('api.not_found', :type=>'barclamp', :id=>params[:id]), :status => 404
    # list (no ID)
    elsif request.get?  
      barclamps = {}
      Barclamp.all.each { |b| barclamps[b.id] = b.name }
      render :json => barclamps
    # Catch
    else
      render :text=>I18n.t('api.unknown_request'), :status => 400
    end
  end
  
  def attribs
    unless params[:version].eql?('2.0')
      render :text=>I18n.t('api.wrong_version', :version=>params[:version]) 
      return
    end
    @attrib = Attrib.find_key(params[:id]) if params[:id]
    
    # POST
    if request.post?
      @attrib = Attrib.create params
      render :json => @attrib
    # PUT (not supported)
    elsif request.put?
      render :text=>I18n.t('api.not_supported', :action=>'PUT', :obj=>'attrib'), :status => 504
    # DELETE
    elsif request.delete? and @attrib
      Attrib.destroy @attrib.id
      render :text=>I18n.t('api.deleted', :id=>@attrib.id, :obj=>'attrib')
    # fall through REST actions (all require ID)
    elsif request.get? and @attrib
      render :json => @attrib
    elsif params[:id]
      render :text=>I18n.t('api.not_found', :type=>'attrib', :id=>params[:id]), :status => 404
    # list (no ID)
    elsif request.get?  
      attribs = {}
      Attrib.all.each { |a| attribs[a.id] = a.name }
      render :json => attribs
    # Catch
    else
      render :text=>I18n.t('api.unknown_request'), :status => 400
    end
  end
  
end

