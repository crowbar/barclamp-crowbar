#
# Copyright 2011-2013, Dell
# Copyright 2013-2014, SUSE LINUX Products GmbH
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#



require 'uri'



class ApplicationController < ActionController::Base
  protect_from_forgery with: :exception
  helper :all



  @@users = nil
  
  before_filter :digest_authenticate, :if => :need_to_auth?

  class << self
    def help_contents
      @help_contents ||= []
    end

    def add_help(method, args=[], http_method = [:get])
      http_method.each { |m|
        help_contents.push({
          method => {
            "args" => args,
            "http_method" => m
          }
        })
      }
    end
  end

  

  def self.set_layout(template = "application")
    layout proc { |controller| 
      if controller.is_ajax? 
        nil
      else
        template
      end
    }
  end
  
  def is_ajax?
    request.xhr?
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
  
  @@auth_load_mutex = Mutex.new
  @@realm = ""
  
  def need_to_auth?()
    return false unless File::exists? "htdigest"
    ip = session[:ip_address] rescue nil
    return false if ip == request.remote_addr
    return true
  end
  
  def digest_authenticate
    load_users()    
    authenticate_or_request_with_http_digest(@@realm) { |u| find_user(u) }
    ## only create the session if we're authenticated
    if authenticate_with_http_digest(@@realm) { |u| find_user(u) }
      session[:ip_address] = request.remote_addr
    end
  end
  
  def find_user(username) 
    return false if !@@users || !username
    user = @@users[username]
    return false unless user
    return user[:password] || false   
  end
  
  ##
  # load the ""user database"" but be careful about thread contention.
  # $htdigest gets flushed when proposals get saved (in case they user database gets modified)
  $htdigest_reload =true
  $htdigest_timestamp = Time.now()
  def load_users
    unless $htdigest_reload
      f = File.new("htdigest")
      if $htdigest_timestamp != f.mtime
        $htdigest_timestamp = f.mtime
        $htdigest_reload = true
      end
    end
    return if @@users and !$htdigest_reload  

    ## only 1 thread should load stuff..(and reset the flag)
    @@auth_load_mutex.synchronize  do
      $htdigest_reload = false if $htdigest_reload   
    end

    ret = {}
    data = IO.readlines("htdigest")
    data.each { |entry|
      next if entry.strip.length ==0
      list = entry.split(":") ## format: user : realm : hashed pass
      user = list[0].strip rescue nil
      password = list[2].strip rescue nil
      realm = list[1].strip rescue nil
      ret[user] ={:realm => realm, :password => password}  
    }
    @@auth_load_mutex.synchronize  do 
        @@users = ret.dup
        @@realm = @@users.values[0][:realm]
    end
    ret
  end

  def flash_and_log_exception(e)
    flash[:alert] = e.message
    log_exception(e)
  end

  def log_exception(e)
    lines = [ e.message ] + e.backtrace
    Rails.logger.warn lines.join("\n")
  end

  def render_not_found
    respond_to do |format|
      format.json { render :json => { :error => "Not found" }, :status => 404 }
    end
  end
end

