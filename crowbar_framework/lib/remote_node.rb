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

require "socket"

module RemoteNode

  # Wait for the host to come up and reach a certain runlevel
  # so we can send ssh commands to it
  # @param host [string] hostname or IP
  # @param timeout [integer] timeout in seconds

  def self.ready?(host, timeout = 60, sleep_time = 10)
    timeout_at = Time.now.to_i + timeout
    ip         = self.resolve_host(host)

    self.wait_until(timeout_at, sleep_time) do
      self.port_open?(ip, 22) && self.ssh_cmd(ip, runlevel_check_cmd(host))
    end
  end

  private

  # Workaround for similar issue
  # http://projects.puppetlabs.com/issues/2776
  def self.resolve_host(host)
    `dig +short #{host}`
  end

  def self.runlevel_check_cmd(host)
    if self.node_redhat?(host)
      'runlevel | grep "N 3"'
    else
      'last | head -n1 | grep -vq down'
    end
  end

  def self.node_redhat?(host)
    nobj = NodeObject.find_node_by_name(host)
    nobj && %w(redhat centos).include?(nobj[:platform])
  end

  # Polls until yielded block returns true or a timeout is reached
  def self.wait_until(timeout_at, sleep_time = 10)
    done = block_given? ? yield : false

    while Time.now.to_i < timeout_at && !done do
      done = block_given? ? yield : false

      unless done
        puts "Waiting, next attempt at #{Time.now + sleep_time}"
        sleep(sleep_time)
      end
    end

    done
  end

  def self.ssh_cmd(host_or_ip, cmd)
    ssh = ["sudo", "-i", "-u", "root", "--", "ssh", "-o", "TCPKeepAlive=no", "-o", "ServerAliveInterval=15", "root@#{host_or_ip}"]
    ssh << cmd
    system(*ssh)
  end

  def self.port_open?(ip, port)
    sock = nil
    begin
      sock = TCPSocket.new(ip, port)
    rescue => e
      false
    else
      sock.close
      true
    end
  end
end
