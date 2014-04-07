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

class ChefController < CrowbarController
  def export
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
        flash[:alert] = t("support.export.fail", error: e.message)
      rescue => e
        log_exception e
      ensure
        FileUtils.rm_rf Rails.root.join("tmp", filename).to_s
      end
    end

    Process.detach(pid)
    redirect_to file_utils_url(waiting: true, file: filename)
  rescue StandardError => e
    log_exception e
    flash[:alert] = I18n.t("support.export.fail", error: e.message)

    redirect_to utils_url
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
