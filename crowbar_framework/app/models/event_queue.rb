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

#
# This is a singleton Class that allows the Rails app to send
# broadcast notifications to AMQP listeners.
#
# Events are defined in the app/model/events directory.
#
# Calling publish the first time will establish the outbound queue and warm the queue.
#
class EventQueue

  @configured = false
  @channel = nil
  @exchange = nil

  def self.build_exchange
    @channel ||= AMQP::Channel.new(AMQP.connection)
    @exchange = @channel.fanout("amqpgem.patterns.events", :durable => true, :auto_delete => false)
    3.times do |i|
      @exchange.publish({"msg"=>"A warmup message #{i} from #{Time.now.strftime('%H:%M:%S %m/%b/%Y')}"}.to_json, :type => "warmup")
    end
  end

  def self.get_channel
    @channel
  end

  def self.get_exchange
    @exchange
  end

  def self.publish(event)
    begin
      build_exchange unless get_channel
      get_exchange.publish(event.to_hash.to_json, :type => event.event_type)
    rescue RuntimeError => e
      t = Thread.new { AMQP.start }
      sleep(1.0)
      build_exchange unless get_channel
      get_exchange.publish(event.to_hash.to_json, :type => event.event_type)
    end
  end
 
end

