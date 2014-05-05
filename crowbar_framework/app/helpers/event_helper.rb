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

module EventHelper
  # def faye_host
  #   "http://#{ENV["CROWBAR_HOST"] || "localhost"}:9292"
  # end

  # def faye_path
  #   "/faye"
  # end

  # def faye_url
  #   [
  #     faye_host,
  #     faye_path
  #   ].join
  # end

  # def faye_script
  #   [
  #     faye_url,
  #     "client.js"
  #   ].join("/")
  # end

  def broadcast(channel, &block)
    # payload = yield

    # message = {
    #   channel: channel, 
    #   data: payload
    # }

    # begin
    #   response = Net::HTTP.post_form(
    #     URI.parse(faye_url), 
    #     message: message.to_json
    #   )

    #   case response.code
    #   when 404
    #     logger.debug "Faye not found at #{faye_url}!"
    #   end
    # rescue Timeout::Error
    #   logger.debug "Timeout while connecting to #{faye_url}!"
    # end

    # pid = Process.fork do
    #   begin
    #     response = Net::HTTP.post_form(
    #       URI.parse(faye_url), 
    #       message: message.to_json
    #     )

    #     case response.code
    #     when 404
    #       logger.debug "Faye not found at #{faye_url}!"
    #     end
    #   rescue Timeout::Error
    #     logger.debug "Timeout while connecting to faye at #{faye_url}!"
    #   rescue => e
    #     logger.debug "Communication to faye failed with #{e.message}"
    #   end
    # end

    # Process.detach(pid)
  end
end
