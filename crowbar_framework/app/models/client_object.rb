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

class ClientObject < ChefObject
  def self.find_client_by_name(name)
    begin
      return ClientObject.new Chef::ApiClient.load(name)
    rescue StandardError => e
      Rails.logger.fatal("Failed to find client: #{name} #{e.message}")
      return nil
    end
  end

  def initialize(client)
    @client = client
  end

  def save
    @client.save
  end

  def destroy
    @client.destroy
  end
end
