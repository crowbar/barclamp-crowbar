# Copyright 2011-2013, Dell
# Copyright 2013, SUSE LINUX Products GmbH
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
# Author: Dell Crowbar Team
# Author: SUSE LINUX Products GmbH
#

require "socket"

module RemoteNode
  def self.ready?(host, timeout = 60)
    sleep(10)

    #workaround for similar issue
    #http://projects.puppetlabs.com/issues/2776

    ip = `dig +short #{host}`
    nobj = NodeObject.find_node_by_name(host)
    start = Time.now.to_i

    while (Time.now.to_i - start) < timeout do
      begin
        puts "checking socket"
        TCPSocket.new(ip, 22)

        if %w(redhat centos).include?(nobj[:platform])
          puts "checking runlevel"
          raise "next" unless system("sudo -i -u root -- ssh root@#{host} 'runlevel | grep \"N 3\"'")
        else
          puts "checking lastlog"
          raise "next" unless system("sudo -i -u root -- ssh root@#{host} 'last | head -n1 | grep -vq down'")
        end
        
        return true
      rescue => e
        puts "Next cycle due to:"
        puts "#{e.inspect}"
        puts "------------"
        
        sleep(10)
        next
      end
    end
    return false
  end
end
