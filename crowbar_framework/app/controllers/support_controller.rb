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
# Author: RobHirschfeld 
# 

class SupportController < ApplicationController
    
  require 'chef'

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
      format.xml  { render :xml => @exports }
      format.json { render :json => @exports }
    end
  end
  
  def upload
    @file = nil
    if request.post?
      if params[:file] 
        begin
          tmpfile = params[:file]
          if tmpfile.class.to_s == 'Tempfile'
            @file = tmpfile.original_filename
            file = File.join import_dir, @file
            File.open(file, "wb") { |f| }
            while blk = tmpfile.read(65536)
              File.open(file, "ab") { |f| f.write(blk) }
            end
            tmpfile.delete
            # update the crowbar.yml for the barclamp
            extract_crowbar_yml(import_dir, @file, true) 
            flash[:notice] = t('.succeeded', :scope=>'support.upload') + ": " + @file
          end
        rescue
          Rails.logger.error("Upload of file failed")
          flash[:notice] = t('.failed', :scope=>'support.upload')
        end
      else
        flash[:notice] = t('.no_file', :scope=>'support.upload')
      end
    end
    redirect_to :action=>'import'
  end
  
  def import
    @installed = ServiceObject.barclamp_catalog['barclamps'] 
    # handle case there we've installed a sub barclamp for the meta, but not the meta
    @installed.delete_if { |k, v| v['order'].nil? }
    @imports = {}
    if request.post?
      bcs = []
      bc_list = []
      importer = File.join '/opt', 'dell', 'bin', 'barclamp_install.rb'
      params.each do |k,v|
        if k =~ /^barclamp_(.*)/
          tar = File.join RAILS_ROOT, import_dir, v
          if File.exist? tar
            bcs << tar 
            bc_list << k.split('_')[1]
          end
        end 
      end
      if bcs.length>0
        barclamps = bcs.map{ |i| '"'+i+'"' }.join(' ')
        begin
          logpath = File.join RAILS_ROOT, 'public', 'export', SERVER_PID+'.import.log'
          %x[sudo #{importer} #{barclamps} > #{logpath}]
          flash[:notice] = "#{t('success', :scope=>'support.import')}: #{bc_list.join(', ')}"
          redirect_to restart_path(:id=>'import')
        rescue
          flash[:notice] = "#{t('error_import', :scope=>'support.import')}: #{bc_list.join(', ')}" 
        end
      else
        flash[:notice] = "#{t('error_file_missing', :scope=>'support.import')}: #{bc_list.join(', ')}" 
      end
    else 
      Dir.entries(import_dir).each do |tar|
        if tar =~ /^.*t(ar\.|)gz$/    # match for tar.gz or tgz 
          name = extract_crowbar_yml(import_dir, tar, false)
          begin
            cb = YAML.load_file(File.join(import_dir, name))
            key = cb['barclamp']['name']
            cb['git'] = { 'date'=> I18n.t('unknown'), 'commit'=> I18n.t('not_set') } if cb['git'].nil?
            @imports[key] = { :tar=>tar, :barclamp=>cb['barclamp'], :date=>cb['git']['date'], :help=>cb['barclamp']['online_help'], :commit=>cb['git']['commit'], :prereq=>[], :requires=>[]}
            if cb['barclamp'].has_key? 'requires'
              cb['barclamp']['requires'].each do |prereq|
                next if prereq =~ /^@/
                @imports[key][:requires] << prereq
              end
            end
            unless @installed.keys.include? key 
              @installed[key] = {:new=>true, :name=>key, 'user_managed'=>(cb['barclamp']['user_managed'] || 'yes'), 'order'=>cb['crowbar']['order']}
            end
          rescue Exception=>e
            # something happened to the YAML file!
            @installed[key] = {:new=>true, :name=>key, 'commit'=>e.message, 'order'=>-1} unless @installed.keys.include? key
            @imports[key] = { :tar=>tar, :barclamp=>key, :date=>I18n.t('error_yml', :scope=>'support.import'), :commit=>I18n.t('na'), :prereq=>[]} unless @imports.keys.include? key
          end
        end
        @imports.each do |key, values|
          unless values[:requires].nil?
            values[:requires].each do |prereq|
              next if @imports[key][:prereq].include? prereq
              @imports[key][:prereq] << prereq if @installed[prereq].nil? or @installed[prereq][:new]
            end
          end
        end
      end
      respond_to do |format|
        format.html # index.html.haml
        format.xml  { render :xml => @imports }
        format.json { render :json => @imports }
      end   
    end 
  end
  
  def export_chef
    if CHEF_ONLINE
      begin
        Dir.entries('db').each { |f| File.delete(File.expand_path(File.join('db',f))) if f=~/.*\.json$/ }
        NodeObject.all.each { |n| n.export }
        RoleObject.all.each { |r| r.export }
        ProposalObject.all.each { |p| p.export }
        @file = cfile ="crowbar-chef-#{Time.now.strftime("%Y%m%d-%H%M%S")}.tgz"
        pid = fork do
          system "tar -czf #{File.join('/tmp',cfile)} #{File.join('db','*.json')}" 
          File.rename File.join('/tmp',cfile), File.join(export_dir,cfile)
        end        
        redirect_to "/utils?waiting=true&file=#{@file}"
      rescue Exception=>e
        flash[:notice] = I18n.t('support.export.fail') + ": " + e.message
        redirect_to "/utils"
      end
    else
      flash[:notice] = 'feature not available in offline mode'
    end
  end
  
  def restart
    @init = false
    if params[:id].nil?
      render
    elsif params[:id].eql? "request" or params[:id].eql? "import"
      @init = true
      render
    elsif params[:id].eql? "in_process"
      %x[sudo bluepill crowbar-webserver restart] unless RAILS_ENV == 'development'
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

end 
