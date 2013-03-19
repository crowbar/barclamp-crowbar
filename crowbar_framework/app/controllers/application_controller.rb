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

require 'uri'
require 'digest/md5'
require 'active_support/core_ext/string'

# Filters added to this controller apply to all controllers in the application.
# Likewise, all the methods added will be available for all controllers.
class ApplicationController < ActionController::Base
  
  
  CB_CONTENT_TYPE19 = "application/vnd.crowbar+json; version=1.9"
  CB_CONTENT_TYPE20 = "application/vnd.crowbar+json; version=2.0"
  
  helper_method :is_dev?
  
  before_filter :crowbar_auth
  
  # Basis for the reflection/help system.
  
  # First, a place to stash the help contents.  
  # Using a class_inheritable_accessor ensures that 
  # these contents are inherited by children, but can be 
  # overridden or appended to by child classes without messing up 
  # the contents we are building here.
  class_attribute :help_contents
  self.help_contents = []
  
  # Class method for adding method-specific help/API information 
  # for each method we are going to expose to the CLI.
  # Since it is a class method, it will not be bothered by the Rails
  # trying to expose it to everything else, and we can call it to build
  # up our help contents at class creation time instead of snapshot creation
  # time, so there is minimal overhead.
  # Since we are just storing an arrray of singleton hashes, adding more
  # user-oriented stuff (descriptions, exmaples, etc.) should not be a problem.
  def self.add_help(method,args=[],http_method=[:get])
    # if we were passed multiple http_methods, build an entry for each.
    # This assumes that they all take the same parameters, if they do not
    # you should call add_help for each different set of parameters that the
    # method/http_method combo can take.
    http_method.each { |m|
      self.help_contents = self.help_contents.push({
        method => {
                                             "args" => args,
                                             "http_method" => m
        }
      })
    }
  end
  
  #helper :all # include all helpers, all the time
  
  protect_from_forgery # See ActionController::RequestForgeryProtection for details
  
  def self.set_layout(template = "application")
    layout proc { |controller| 
      if controller.is_ajax? 
        return nil
      end
      template
    }
  end

  # needed by Devise auth
  def is_ajax?
    request.xhr?
  end
  
  # formats API json output 
  # using this makes it easier to update the API format for all models
  def api_index(type, list, link=nil)
    if params[:version].eql?('v2') 
      link ||= eval("#{type.to_s.pluralize(0)}_path")   # figure out path from type
      return {:json=>{:list=>list, :count=>list.count, :type=>type, :link=>link}, :content_type=>CB_CONTENT_TYPE19 }
    else
      return {:text=>I18n.t('api.wrong_version', :version=>params[:version])}
    end
  end

  # formats API json for output
  # using this makes it easier to update the API format for all models
  def api_show(type, type_class, key=nil, link=nil, o=nil)
    if params[:version].eql?('v2') 
      # we've got information to move forward
      key ||= o.id unless o.nil?
      key ||= params[:id]
      link ||= request.path # figure out path from type
      o ||= type_class.find_key key
      if o
        return {:json=>{:item=>o, :type=>type, :link=>link}, :content_type=>CB_CONTENT_TYPE19}
      else
        return {:text=>I18n.t('api.not_found', :id=>key, :type=>type.to_s), :status => :not_found}
      end
    else
      return {:text=>I18n.t('api.wrong_version', :version=>params[:version])}
    end
  end
    
  # formats API for delete
  # using this makes it easier to update the API format for all models
  def api_delete(type, key=nil)
    if params[:version].eql?('v2') 
      key ||= params[:id]
      type.delete type.find_key(key)
      return {:text=>I18n.t('api.deleted', :id=>key, :obj=>'jig')}
    else
      return {:text=>I18n.t('api.wrong_version', :version=>params[:version])}
    end
  end
  
  def api_update(type, type_class, key=nil)
    key ||= params[:id]
    o = type_class.find_key key
    if o
      o.update_attribs params
      return api_show type, type_class, nil, nil, o
    else
      return {:text=>I18n.t('api.not_found', :id=>key, :type=>type.to_s), :status => :not_found}
    end
  end

  def api_not_supported(verb, object)
    return {:text=>I18n.t('api.not_supported', :verb=>verb.upcase, :obj=>object), :status => 405}
  end
  
  # shared routine that finds the barclamp for other base calls (e.g.: barclampInstance, config & role)
  def barclamp
    name = params[:barclamp]    # fall through routes specify the barclamp
    name ||= $1 if params[:controller] =~ /^barclamp_([a-z][_a-z0-9]*)/
    @barclamp = Barclamp.find_by_name name if @barclamp.nil? or !@barclamp.name.eql? name
    @barclamp
  end
  
  add_help(:help)
  def help
    render :json => { self.controller_name => self.help_contents.collect { |m|
        res = {}
        m.each { |k,v|
          # sigh, we cannot resolve url_for at class definition time.
          # I suppose we have to do it at runtime.
          url=URI::unescape(url_for({ :action => k,
                        :controller => self.controller_name,
            
          }.merge(v["args"].inject({}) {|acc,x|
            acc.merge({x.to_s => "(#{x.to_s})"})
          }
          )
          ))
          res.merge!({ k.to_s => v.merge({"url" => url})})
        }
        res
      }
    }
  end
  set_layout
  
  #########################
  # private stuff below.
  
  private  

  def digest_auth!
    authenticate_or_request_with_http_digest(User::DIGEST_REALM) do |username|
      u = User.find_by_username(username)
      session[:digest_user] = u.username
      u.encrypted_password
    end
    warden.custom_failure! if performed?
  end
  
  #return true if we digest signed in
  def crowbar_auth
    case
    when current_user then authenticate_user!
    when request.headers["HTTP_AUTHORIZATION"] && request.headers["HTTP_AUTHORIZATION"].starts_with?('Digest username=') then digest_auth!
    when ActionDispatch::Request::LOCALHOST.any?{|i| 
        if i.is_a?(Regexp)
          i =~ request.ip
        else
          i == request.ip
        end
      } && File.exists?("/tmp/.crowbar_in_bootstrap") && File.stat("/tmp/.crowbar_in_bootstrap").uid == 0
      current_user = User.find_by_id_or_username("crowbar")
      true
    else
      respond_to do |format|
        format.html { authenticate_user!  }
        format.json { digest_auth!  }
      end
    end
  end
end
