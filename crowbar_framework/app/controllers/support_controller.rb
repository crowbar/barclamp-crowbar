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

class SupportController < ApplicationController
  def logs
    filename = "crowbar-logs-#{Time.now.strftime("%Y%m%d-%H%M%S")}.tar.bz2"
    system("sudo", "-i", Rails.root.join("..", "bin", "gather_logs.sh").expand_path, filename)
    redirect_to "/export/#{filename}"
  end

  def get_cli
    executable = Rails.root.join("..", "bin", "gather_cli.sh").expand_path
    system("sudo", "-i", executable, request.env['SERVER_ADDR'], request.env['SERVER_PORT'])
    redirect_to "/crowbar-cli.tar.gz"
  end

  def index
    @export = Hashie::Mash.new({
      :waiting => params[:waiting] == "true" || params[:format] == "json",
      :counter => 0,
      :current => params["file"].to_s.gsub("-DOT-", "."),
      :files => {
        :log => [],
        :cli => [],
        :chef => [],
        :other => [],
        :supportconfig => [],
        :import => []
      }
    })

    export_dir.children.each do |file|
      filename = file.basename.to_s

      if filename =~ /^\./
        next
      elsif filename =~ /^KEEP_THIS.*/
        next
      elsif filename =~ /^crowbar-logs-.*/
        @export.files.log.push filename
      elsif filename =~ /^crowbar-cli-.*/
        @export.files.cli.push filename
      elsif filename =~ /^crowbar-chef-.*/
        @export.files.chef.push filename
      elsif filename =~ /(.*).import.log$/
        @export.files.import.push filename
      elsif filename =~ /^supportconfig.*/
        @export.files.supportconfig.push filename
      else
        @export.files.other.push filename
      end

      @export.waiting = false if filename == @export.current
      @export.counter += 1
    end

    respond_to do |format|
      format.html
      format.json { render :json => @export.to_json }
    end
  end

  def destroy
    file = check_dir("export").join(params[:id])

    begin
      file.unlink
      flash[:notice] = t("support.index.delete_succeeded", :file => file.basename)
    rescue
      flash[:alert] = t("support.index.delete_failed", :file => file.basename)
    end

    redirect_to utils_url
  end

  def supportconfig
    base = "supportconfig-#{Time.now.strftime("%Y%m%d-%H%M%S")}"
    filename = "#{base}.tbz"

    pid = Process.fork do
      export = Cocaine::CommandLine.new("sudo", "-i supportconfig -Q -R :destination")
      chown = Cocaine::CommandLine.new("sudo", "-i chown -R #{Process.uid}:#{Process.gid} :destination")

      begin
        export.run(
          destination: Rails.root.join("tmp", base).to_s
        )

        chown.run(
          destination: Rails.root.join("tmp", base).to_s
        )

        File.rename Dir[Rails.root.join("tmp", base, "*.tbz").to_s].first, export_dir.join(filename).to_s
      rescue Cocaine::ExitStatusError => e
        flash[:alert] = t("support.export.fail", :error => e.message)
      rescue => e
        log_exception e
      ensure
        FileUtils.rm_rf Rails.root.join("tmp", base).to_s
      end
    end

    Process.detach(pid)
    redirect_to utils_url(:waiting => true, :file => filename)
  rescue StandardError => e
    log_exception e
    flash[:alert] = t("support.export.fail", :error => e.message)

    redirect_to utils_url
  end

  def chef
    Rails.root.join("db").children.each do |file|
      file.unlink if file.extname == ".json"
    end

    NodeObject.all.map(&:export)
    RoleObject.all.map(&:export)
    ProposalObject.all.map(&:export)

    base = "crowbar-chef-#{Time.now.strftime("%Y%m%d-%H%M%S")}"
    filename = "#{base}.tgz"

    pid = Process.fork do
      sources = Rails.root.join("db").children.map do |file|
        file.basename if file.extname == ".json"
      end.compact

      export = Cocaine::CommandLine.new("tar", "-C :path -czf :destination #{sources.join(" ")}")

      begin
        export.run(
          path: Rails.root.join("db").to_s, 
          destination: Rails.root.join("tmp", filename).to_s
        )

        File.rename Rails.root.join("tmp", filename).to_s, export_dir.join(filename).to_s
      rescue Cocaine::ExitStatusError => e
        flash[:alert] = t("support.export.fail", :error => e.message)
      rescue => e
        log_exception e
      ensure
        FileUtils.rm_rf Rails.root.join("tmp", filename).to_s
      end
    end

    Process.detach(pid)
    redirect_to utils_url(:waiting => true, :file => filename)
  rescue StandardError => e
    log_exception e
    flash[:alert] = I18n.t("support.export.fail", :error => e.message)

    redirect_to utils_url
  end

  def restart
    @init = false
    @log = Rails.root.join("public", "export", "#{Crowbar::Application::SERVER_PID}.import.log")

    if params[:id].nil?
      render
    elsif params[:id].eql? "request" or params[:id].eql? "import"
      @init = true
      render
    elsif params[:id].eql? "in_process"
      %x[sudo bluepill crowbar-webserver restart] unless Rails.env.development?
      render :json => true
    elsif params[:id].eql? Crowbar::Application::SERVER_PID
      render :json => false
    elsif !params[:id].eql? Crowbar::Application::SERVER_PID
      render :json => true
    else
      render
    end
  end

  protected

  def import_dir
    check_dir "import"
  end

  def export_dir
    check_dir "export"
  end

  def check_dir(type)
    path = Rails.root.join("public", type)

    unless path.directory?
      FileUtil.mkdir_p path.expand_path
    end

    path
  end
end
