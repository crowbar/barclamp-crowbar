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

class EventQueue

  @@channel = nil
  @@exchange = nil

  def self.build_exchange
    t = Thread.new { AMQP.start }
    sleep(1.0)

    EventMachine.next_tick do
      @@channel ||= AMQP::Channel.new(AMQP.connection)
      @@exchange = @@channel.fanout("amqpgem.patterns.events", :durable => true, :auto_delete => false)
      3.times do |i|
        @@exchange.publish({"msg"=>"A warmup message #{i} from #{Time.now.strftime('%H:%M:%S %m/%b/%Y')}"}.to_json, :type => "warmup")
      end
    end
  end

  def self.publish_event(type, msg)
    EventQueue.build_exchange unless @@channel
    @@exchange.publish({"msg"=>msg}.to_json, :type => type)
  end
 
end

