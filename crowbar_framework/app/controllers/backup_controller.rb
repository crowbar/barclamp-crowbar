#
# Copyright 2015, SUSE LINUX GmbH
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

class BackupController < ApplicationController
  #
  # Backup
  #
  # Provides the restful api call for
  # /utils/backup   POST   Trigger a backup
  def backup
    @backup = Backup.new
    respond_to do |format|
      if !@backup.nil? && @backup.save
        format.json { head :ok }
        format.html { redirect_to backup_path }
      else
        msg = I18n.t(".invalid_filename", scope: "backup")
        format.json { render json: msg }
        format.html do
          flash[:alert] = msg
          redirect_to backup_path
        end
      end
    end
  end

  #
  # Download
  #
  # Provides the restful api call for
  # /utils/backup/download/:name/:created_at 	GET 	Download a backup
  def download
    @backup = Backup.new(
      name: params[:name],
      created_at: params[:created_at]
    )
    respond_to do |format|
      if @backup.path.exist?
        format.any do
          send_file(
            @backup.path,
            filename: "#{params[:name]}-#{params[:created_at]}.tar.gz"
          )
        end
      else
        msg = I18n.t(".missing_backup", scope: "backup")
        format.html do
          flash[:alert] = msg
          redirect_to backup_path
        end
        format.json { render json: msg }
      end
    end
  end

  #
  # Delete Backup
  #
  # Provides the restful api call for
  # data-confirm method delete
  # /utils/backup/delete 	DELETE 	Delete a backup
  def delete
    @backup = Backup.new(
      name: params[:name],
      created_at: params[:created_at]
    )

    respond_to do |format|
      if @backup.valid?
        @backup.delete

        format.json { head :ok }
        format.html { redirect_to backup_path }
      else
        format.json { render json: @backup.errors }
        format.html { flash[:alert] = @backup.errors }
      end
    end
  end
end
