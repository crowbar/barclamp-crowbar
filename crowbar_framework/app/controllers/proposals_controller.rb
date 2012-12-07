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
require 'json'

class ProposalsController < ApplicationController

  self.help_contents = Array.new(superclass.help_contents)

  add_help(:status,[:id],[:get])
  def status
    proposals = {}
    i18n = {}
    i18n['unknown'] = I18n.t 'unknown', :scope=>'proposal.status', :default=>'Unknown'
    error = ""
    count = 0

    props = Proposal.find_keys params[:id]
    
    unless props.nil?
      begin
        props.each do |prop|
          i18n[prop.status] = I18n.t(prop.status, :scope=>'proposal.status', :default=>prop.status.humanize) unless i18n.has_key? prop.status
          proposals[prop.id] = prop.status
        end
        count = proposals.size
      rescue Exception=>e
        count = -1
        error = e.message
        Rails.logger.fatal("Failed to iterate over proposal list due to '#{error}'")
      end
    end

    render :inline => {:proposals=>proposals, :i18n=>i18n, :count=>count, :error=>error}.to_json, :cache => false

  end

end

