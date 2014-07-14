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

    self.wait_for_uptime(host)

    self.wait_until(timeout_at, sleep_time) do
      self.port_open?(ip, 22) && self.ssh_cmd(ip, runlevel_check_cmd(host))
    end
  end

  private

  # When rebooting a host it's possible to login to the host before the host restarts.
  # That's why we wait until it's impossible to get the uptime or the current uptime is
  # lower than the first uptime.
  def self.wait_for_uptime(host_or_ip)
    uptime_first = self.ssh_cmd_get_uptime(host_or_ip)
    30.times do
      if uptime_first > 0 and self.ssh_cmd_get_uptime(host_or_ip) >= uptime_first
        sleep(10)
      else
        break
      end
    end
  end

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

  # Basic ssh parameters used for different commands on the remote host
  def self.ssh_cmd_base(host_or_ip)
    return ["sudo", "-i", "-u", "root", "--", "ssh", "-o", "TCPKeepAlive=no", "-o", "ServerAliveInterval=15", "root@#{host_or_ip}"]
  end

  # get uptime of the given host or 0 if command can not be executed
  def self.ssh_cmd_get_uptime(host_or_ip)
    ssh_cmd = self.ssh_cmd_base(host_or_ip)
    ssh_cmd << "cat" << "/proc/uptime"
    retstr = `#{ssh_cmd.join(" ")}`.split(" ")[0].to_i
    if $?.exitstatus != 0
      retstr = 0
    end
    return retstr
  end

  def self.ssh_cmd(host_or_ip, cmd)
    ssh = self.ssh_cmd_base(host_or_ip)
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
