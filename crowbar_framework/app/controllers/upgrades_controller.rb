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

class UpgradesController < ApplicationController

  def show
  end

  def prepare_nodes
    service_object = CrowbarService.new(Rails.logger)

    # transition nodes to "crowbar_upgrade"
    service_object.prepare_nodes_for_crowbar_upgrade

    # create crowbar dump
    @backup = Backup.new
    @backup.save
    
    redirect_to upgrade_path
  end

  def revert_nodes
    service_object = CrowbarService.new(Rails.logger)

    # transition nodes to "crowbar_upgrade"
    service_object.revert_nodes_from_crowbar_upgrade

    redirect_to upgrade_path
  end
end
