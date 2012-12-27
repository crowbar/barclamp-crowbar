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
      :actions=>['node','group','cmdb', 'attrib'],
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
    @attrib = Attrib.find_key(params[:target_id]) if params[:target_id]
    # POST
    if request.post?
      @node.attribs << @attrib unless @node.attribs.include? @attrib
      render :json => {:node=>@node.id, :attrib=>@attrib.id, :name=>@attrib.name, :description=>@attrib.description}
    # PUT (not supported)
    elsif request.put?
      throw 'not implemented'
      render :text=>I18n.t('api.not_supported', :action=>'PUT', :obj=>'node_attrib'), :status => 504
    # DELETE
    elsif request.delete? and @attrib
      @node.attribs.delete @attrib
      render :text=>I18n.t('api.deleted', :id=>@attrib.id, :obj=>'node_attrib')
    # fall through REST actions (all require ID)
    elsif request.get? and @attrib
      # MISSING - get actual value!!! 
      render :json => {:node=>@node.id, :attrib=>@attrib.id, :value=>'unknown', :last_updated=>'unknown'}
    elsif params[:target_id]
      render :text=>I18n.t('api.not_found', :type=>'node_attrib', :id=>params[:target_id]), :status => 404
    # list (no ID)
    elsif request.get?  
      attribs = {}
      @node.attribs.each { |a| attribs[a.id] = a.name }
      render :json => attribs
    # Catch
    else
      render :text=>I18n.t('api.unknown_request'), :status => 400
    end
    
  end
  
  def cmdb
    unless params[:version].eql?('2.0')
      render :text=>I18n.t('api.wrong_version', :version=>params[:version]) 
      return
    end
    @cmdb = Cmdb.find_key(params[:id]) if params[:id]
    
    # POST
    if request.post?
      @cmdb = Cmdb.create params
      render :json => @cmdb
    # PUT (not supported)
    elsif request.put?
      render :text=>I18n.t('api.not_supported', :action=>'PUT', :obj=>'cmdb'), :status => 504
    # DELETE
    elsif request.delete? and @cmdb
      Cmdb.delete @cmdb.id
      render :text=>I18n.t('api.deleted', :id=>@cmdb.id, :obj=>'cmdb')
    # fall through REST actions (all require ID)
    elsif request.get? and @cmdb
      render :json => @cmdb
    elsif params[:id]
      render :text=>I18n.t('api.not_found', :type=>'cmdb', :id=>params[:id]), :status => 404
    # list (no ID)
    elsif request.get?  
      cmdbs = {}
      Cmdb.all.each { |c| cmdbs[c.id] = c.name }
      render :json => cmdbs
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
  
  def attrib
    unless params[:version].eql?('2.0')
      render :text=>I18n.t('api.wrong_version', :version=>params[:version]) 
      return
    end
    @attrib = Attribute.find_key(params[:id]) if params[:id]
    
    # POST
    if request.post?
      @attrib = Attribute.create params
      render :json => @attrib
    # PUT (not supported)
    elsif request.put?
      render :text=>I18n.t('api.not_supported', :action=>'PUT', :obj=>'attrib'), :status => 504
    # DELETE
    elsif request.delete? and @attrib
      Attribute.delete @attrib.id
      render :text=>I18n.t('api.deleted', :id=>@attrib.id, :obj=>'attrib')
    # fall through REST actions (all require ID)
    elsif request.get? and @attrib
      render :json => @attrib
    elsif params[:id]
      render :text=>I18n.t('api.not_found', :type=>'attrib', :id=>params[:id]), :status => 404
    # list (no ID)
    elsif request.get?  
      attribs = {}
      Attribute.all.each { |a| attribs[a.id] = a.name }
      render :json => attribs
    # Catch
    else
      render :text=>I18n.t('api.unknown_request'), :status => 400
    end
  end
  
end

