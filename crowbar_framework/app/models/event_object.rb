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

module EventObject
  extend self

  def broadcast(subject, payload = {})
    defaults = {
      subject: subject
    }

    exchange.publish(
      payload.merge(defaults), 
      routing_key: queue.name
    )

    disconnect
  end

  def retrieve(&block)
    queue.subscribe(&block)
  end

  def connection
    @connection ||= begin
      c = Bunny.new
      c.start

      c
    end

    unless @connection.connected?
      @connection.start
    end

    @connection
  end

  def disconnect
    connection.close
  end

  def channel
    @channel ||= begin
      connection.create_channel
    end
  end

  def queue
    @queue ||= begin
      channel.queue(
        "crowbar.messages",
        auto_delete: true
      )
    end
  end

  def exchange
    @exchange ||= begin
      channel.default_exchange
    end
  end
end
