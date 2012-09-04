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

require 'digest/md5'

class DigestController < ApplicationController
  
  # This is a hack just to get use restarted on digest!
  REALM = "Crowbar - By selecting OK are agreeing to the License Agreement"
  USERS = { "crowbar" => Digest::MD5.hexdigest(["crowbar",REALM,"crowbar"].join(":"))}  #ha1 digest password

  skip_before_filter :authenticate_user!
  before_filter :authenticate

  # Will only show this page if you digest auth
  def index
    render :text => t('user.digest_success', :default=>'success')
  end

  private
  
    # does the magic auth
    def authenticate
      authenticate_or_request_with_http_digest(REALM) do |username|
        USERS[username]
      end
    end
end