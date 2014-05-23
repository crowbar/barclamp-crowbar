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

class NotificationsController < ApplicationController
  include ActionController::Live

  def index
    response.content_type = "text/event-stream"

    begin

      #loop do
      #  logger.debug "Sending event"
      #  broadcast({ time: Time.now }, { foo: "testing.bar" })
      #  sleep 1
      #end

      # EventObject.retrieve do |delivery_info, metadata, payload|
      #   puts delivery_info.inspect
      #   puts metadata.inspect
      #   puts "Received #{payload.inspect}"

      #   broadcast payload
      # end

    rescue IOError => e
      logger.debug "Client #{request.remote_addr} closed SSE connection"
    ensure
      response.stream.close
      EventObject.disconnect
    end
  end

  protected

  def broadcast(data)
    response.stream.write "data: #{JSON.dump(data)}\n\n"
  end
end
