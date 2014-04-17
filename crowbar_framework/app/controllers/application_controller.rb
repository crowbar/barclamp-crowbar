# -*- encoding : utf-8 -*-
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

class ApplicationController < ActionController::Base
  protect_from_forgery with: :exception
  layout :detect_layout

  before_action :authenticate, if: :authenticate?
  after_action :xsrf_cookie

  class << self
    def help_contents
      @@help_contents ||= []
    end

    def add_help(method, args = [], http_method = [:get])
      http_method.each do |m|
        help_contents.push(
          method => {
            "args" => args,
            "http_method" => m
          }
        )
      end
    end

    def digest_users
      if digest_valid?
        @digest_users ||= digest_loader
      else
        @digest_users = digest_loader
      end
    end

    def digest_realm
      @digest_realm ||= "Crowbar"
    end

    def digest_realm=(value)
      @digest_realm = value
    end

    def digest_stamp
      @digest_stamp ||= Time.now
    end

    def digest_stamp=(value)
      @digest_stamp = value
    end

    def digest_database
      @digest_database ||= Rails.root.join("htdigest")
    end

    def digest_loader
      {}.tap do |users|
        digest_database.open("r") do |file|
          file.each_line do |line|
            next if line.strip.empty?
            next if line.strip[0] == "#"

            user, realm, password = line.split(":", 3).map(&:strip)

            users[user] = {
              realm: realm,
              password: password
            }  
          end
        end

        self.digest_stamp = digest_database.mtime
        self.digest_realm = users.values.first[:realm] rescue "Crowbar"
      end
    end

    def digest_valid?
      digest_database.mtime == digest_stamp
    end
  end

  add_help(:help)
  def help
    response = { 
      self.controller_name => self.class.help_contents.collect do |m|
        {}.tap do |res|
          m.each do |k, v|
            basically = { 
              controller: self.controller_name,
              action: k,
            }

            injected = v["args"].inject({}) do |acc, x|
              acc.merge({x.to_s => "(#{x.to_s})"})
            end

            route = if k =~ /_(path|url)$/
              send(k, injected)
            else
              url_for(
                basically.merge(injected)
              )
            end

            url = URI::unescape(
              route
            )

            res.merge!({ 
              k.to_s.gsub(/_(path|url)$/, "") => v.merge({ 
                "url" => url 
              })
            })
          end
        end
      end
    }

    respond_to do |format|
      format.html { render json: response }
      format.json { render json: response }
      format.xml { render xml: response }
    end
  end

  protected

  def authenticate?
    return false unless self.class.digest_database.file?

    if session[:ip_address] == request.remote_addr
      false
    else
      true
    end
  end

  def authenticate
    self.class.digest_users

    authed = authenticate_or_request_with_http_digest(
      self.class.digest_realm
    ) do |username, password|
      self.class.digest_users[username][:password] rescue false
    end

    if authed
      session[:ip_address] = request.remote_addr
    else
      request_http_digest_authentication(self.class.digest_realm, "Authentication failed")
    end
  end

  def detect_layout
    if request.xhr?
      false
    else
      "application"
    end
  end

  def xsrf_cookie
    cookies['XSRF-TOKEN'] = form_authenticity_token if protect_against_forgery?
  end

  def verified_request?
    valid_token = form_authenticity_token == request.headers['X-XSRF-TOKEN']
    valid_ctype = %w(application/json application/xml text/xml).include? request.content_type

    super || valid_token || valid_ctype
  end

  def log_exception(e)
    lines = [ e.message ] + e.backtrace
    Rails.logger.warn lines.join("\n")
  end

  def flash_exception(e)
    flash[:alert] = e.message
    log_exception(e)
  end

  def render_not_found
    respond_to do |format|
      format.json { render json: { error: "Not found" }, status: 404 }
    end
  end
end
