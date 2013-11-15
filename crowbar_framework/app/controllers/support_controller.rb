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


class SupportController < ApplicationController

  skip_before_filter :crowbar_auth, :only => :digest
  before_filter :digest_auth!, :only => :digest

  # used to pass a string into the debug logger to help find specificall calls  
  def marker
    Rails.logger.info "\nMARK >>>>> #{params[:id]} <<<<< KRAM\n"
    render :text=>params[:id]
  end

  def digest
    if session[:digest_user]
      render :text => t('user.digest_success', :default=>'success')
    else
      render :text => "digest", :status => :unauthorized
    end
  end

  def fail
    raise I18n.t('chuck_norris')
  end

  # used to lookup localization values
  def i18n
    begin
      render :text=>I18n.translate(params[:id], :raise => I18n::MissingTranslationData)
    rescue I18n::MissingTranslationData
      render :text=>"No translation for #{params[:id]}", :status => 404
    end
  end

  # Legacy Support (UI version moved to loggin barclamp)
  def logs
    @file = "crowbar-logs-#{ctime}.tar.bz2"
    system("sudo -i /opt/dell/bin/gather_logs.sh #{@file}")
    redirect_to "/export/#{@file}"
  end
  
  def get_cli
    system("sudo -i /opt/dell/bin/gather_cli.sh #{request.env['SERVER_ADDR']} #{request.env['SERVER_PORT']}")
    redirect_to "/crowbar-cli.tar.gz"
  end

  def import
    @barclamps = Barclamp.all
  end

  def index
    @waiting = params['waiting'] == 'true'
    remove_file 'export', params[:id] if params[:id]
    @exports = { :count=>0, :logs=>[], :cli=>[], :chef=>[], :other=>[], :bc_import=>[] }
    Dir.entries(export_dir).each do |f|
      if f =~ /^\./
        next # ignore rest of loop
      elsif f =~ /^KEEP_THIS.*/
        next # ignore rest of loop
      elsif f =~ /^crowbar-logs-.*/
        @exports[:logs] << f 
      elsif f =~ /^crowbar-cli-.*/
        @exports[:cli] << f
      elsif f =~ /^crowbar-chef-.*/
        @exports[:chef] << f 
      elsif f =~ /(.*).import.log$/
        @exports[:bc_import] << f 
      else
        @exports[:other] << f
      end
      @exports[:count] += 1
      @file = params['file'] if params['file']
      @waiting = false if @file == f
    end
    respond_to do |format|
      format.html # index.html.haml
      format.json { render :json => @exports }
    end
  end

  def bootstrap
    if request.post?
      # only create if no other netwroks
      if BarclampNetwork::Network.count == 0
        deployment = Deployment.system_root.first
        BarclampNetwork::Network.transaction do
          net = BarclampNetwork::Network.create :name=>'admin', :description=>I18n.t('support.bootstrap.admin_net'),  :deployment_id=>deployment.id, :conduit=>'1g0', :v6prefix => "auto"
          BarclampNetwork::Range.create :name=>'admin', :network_id=>net.id, :first=>"192.168.124.10/24", :last=>"192.168.124.11/24"
          BarclampNetwork::Range.create :name=>'dhcp', :network_id=>net.id, :first=>"192.168.124.21/24", :last=>"192.168.124.80/24"
          BarclampNetwork::Range.create :name=>'host', :network_id=>net.id, :first=>"192.168.124.81/24", :last=>"192.168.124.254/24"
        end
      end
    end
    @roles = []
    @roles << Role.find_key("dns-server")
    @roles << Role.find_key("ntp-server")
    @roles << Role.find_key("network-server")
    @roles << Role.find_key("chef-server")
    @roles << Role.find_key("network-admin") || :create_network_admin
  end

  def restart
    @init = false
    if params[:id].nil?
      render
    elsif params[:id].eql? "request" or params[:id].eql? "import"
      @init = true
      render
    elsif params[:id].eql? "in_process"
      %x[sudo bluepill crowbar-webserver restart] unless Rails.env == 'development'
      render :json=>false
    elsif params[:id].eql? SERVER_PID
      render :json=>false
    elsif !params[:id].eql? SERVER_PID
      render :json=>true
    else
      render
    end
  end
  
  private 
  
  def ctime
    Time.now.strftime("%Y%m%d-%H%M%S")
  end
  
  def import_dir
    check_dir 'import'
  end
  
  def export_dir
    check_dir 'export'
  end

  def check_dir(type)
    d = File.join 'public', type
    unless File.directory? d
      Dir.mkdir d
    end
    return d    
  end
  
  def remove_file(type, name)
    begin
      f = File.join(check_dir(type), name)
      File.delete f
      flash[:notice] = t('support.index.delete_succeeded') + ": " + f
    rescue
      flash[:notice] = t('support.index.delete_failed') + ": " + f
    end
  end
  
  def extract_crowbar_yml(import_dir, tar, regen = true)
    archive = File.join import_dir,tar
    name = tar[/^(.*).tar.gz$/,1]
    name = tar[/^(.*).tgz$/,1] if name.nil?   #alternate ending
    name += '.yml'
    # extra from tar if not present
    if regen or !File.exist?(File.join(import_dir, name))
      crowbar = %x[tar -t -f #{archive} | grep crowbar.yml].strip
      %x[tar -x #{crowbar} -f #{archive} -O > #{File.join(import_dir, name)}] if crowbar
    end
    return name
  end
  
  def do_auth!
    case
    when request.fullpath.index("/get_cli") || request.full_path.index("/logs") then digest_auth!
    else
      super
    end
  end
end 
