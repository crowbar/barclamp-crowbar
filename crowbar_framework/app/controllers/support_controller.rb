# Copyright 2011, Dell 
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
# Author: RobHirschfeld 
# 

class SupportController < ApplicationController
  
  require 'chef'
  
  def logs
    system("sudo -i /opt/dell/bin/gather_logs.sh #{ctime}")
    redirect_to "/logs/crowbar-logs-#{ctime}.tar.bz2"
  end
  def get_cli
    system("sudo -i /opt/dell/bin/gather_cli.sh #{request.env['SERVER_ADDR']} #{request.env['SERVER_PORT']}")
    redirect_to "/crowbar-cli.tar.gz"
  end
  
  def index
    if params[:id]
      begin
        f = File.join(export_dir, params[:id])
        f = f.sub(/-DOT-/,'.')
        File.delete f
        flash[:notice] = t('support.index.delete_succeeded') + ": " + f
      rescue
        flash[:notice] = t('support.index.delete_failed') + ": " + f
      end
    end
    @exports = { :logs=>[], :cli=>[], :chef=>[], :other=>[] }
    Dir.entries(export_dir).each do |f|
      if f =~ /^\./
        # ignore
      elsif f =~ /^crowbar-logs-.*/
        @exports[:logs] << f 
      elsif f =~ /^crowbar-cli-.*/
        @exports[:cli] << f
      elsif f =~ /^crowbar-chef-.*/
        @exports[:chef] << f 
      else
        @exports[:other] << f
      end
    end
  end

  def export_chef
    if CHEF_ONLINE
      begin
        Dir.entries('db').each { |f| File.delete(File.expand_path(File.join('db',f))) if f=~/.*\.json$/ }
        NodeObject.all.each { |n| NodeObject.dump n, 'node', n.name }
        RoleObject.all.each { |r| RoleObject.dump r, 'role', r.name }
        ProposalObject.all.each { |p| ProposalObject.dump p, 'data_bag_item_crowbar-bc', p.name[/bc-(.*)/,1] }
        @file = cfile ="crowbar-chef-#{Time.now.strftime("%Y%m%d-%H%M%S")}.tgz"
        system "tar -czf #{File.join(export_dir,cfile)} #{File.join('db','*.json')}" 
        flash[:notice] = I18n.t('support.export.succeed') + ": " + cfile
      rescue Exception=>e
        flash[:notice] = I18n.t('support.export.fail') + ": " + e.message
      end
    end
    index
    render 'index'
  end
  
  private 
  
  def ctime
    Time.now.strftime("%Y%m%d-%H%M%S")
  end
  
  def export_dir
    d = File.join 'public', 'export'
    unless File.directory? d
      Dir.mkdir d
    end
    return d
  end
  
end 
