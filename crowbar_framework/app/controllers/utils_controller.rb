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

class UtilsController < ApplicationController
  def index
    @export = Hashie::Mash.new({
      :waiting => params[:waiting] == "true" || params[:format] == "json",
      :counter => 0,
      :current => params["file"].to_s.gsub("-DOT-", "."),
      :files => {
        :log => [],
        :chef => [],
        :supportconfig => [],
        :other => []
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
      elsif filename =~ /^crowbar-chef-.*/
        @export.files.chef.push filename
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
    file = export_dir.join(params[:id])

    begin
      file.unlink

      respond_to do |format|
        format.html do
          flash[:notice] = t("utils.index.delete_succeeded", file: file.basename)
          redirect_to utils_url
        end

        format.json { head :ok }
      end
    rescue
      respond_to do |format|
        format.html do
          flash[:alert] = t("utils.index.delete_failed", file: file.basename)
          redirect_to utils_url
        end

        format.json { render json: { error: t("utils.index.delete_failed", file: file.basename) }, status: 500 }
      end
    end
  end

  protected

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
