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
    msg = "Waiting for host, next attempt at #{Time.now + sleep_time}"

    nobj = NodeObject.find_node_by_name(host)
    reboot_requesttime = nobj[:crowbar_wall][:wait_for_reboot_requesttime] || 0

    self.wait_until(msg, timeout_at, sleep_time) do
      self.port_open?(ip, 22) && self.reboot_done?(ip, reboot_requesttime) && self.ssh_cmd(ip, runlevel_check_cmd(host))
    end
  end

  # wait until no other chef-clients are currently running on the host
  def self.chef_ready?(host, timeout = 60, sleep_time = 10)
    timeout_at = Time.now.to_i + timeout
    ip         = self.resolve_host(host)
    msg = "Waiting for already running chef-client, next attempt at #{Time.now + sleep_time}"
    self.wait_until(msg, timeout_at, sleep_time) do
      self.port_open?(ip, 22) && !self.chef_clients_running?(ip)
    end
  end

  private

  # are there some chef-clients running on the host?
  # chef-client daemon started by pid 1 is not counted
  def self.chef_clients_running?(host_or_ip)
    ssh_cmd = self.ssh_cmd_base(host_or_ip)
    ssh_cmd << "'if test -f /var/chef/cache/chef-client-running.pid; then flock -n /var/chef/cache/chef-client-running.pid true; fi'"
    ssh_cmd = ssh_cmd.map{|x| x.chomp}.join(" ")
    res = `#{ssh_cmd}`
    case $?.exitstatus
    when 0
      return false
    else
      return true
    end
  end

  def self.reboot_done?(host_or_ip, reboot_requesttime)
    # if reboot_requesttime is zero, we don't know when the reboot was requested.
    # in this case return true so the process can continue
    if reboot_requesttime > 0
      boottime = self.ssh_cmd_get_boottime(host_or_ip)
      if boottime > 0 and boottime <= reboot_requesttime
        false
      else
        true
      end
    else
      true
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
  def self.wait_until(msg, timeout_at, sleep_time = 10)
    done = block_given? ? yield : false

    while Time.now.to_i < timeout_at && !done do
      done = block_given? ? yield : false

      unless done
        puts msg
        sleep(sleep_time)
      end
    end

    done
  end

  # Basic ssh parameters used for different commands on the remote host
  def self.ssh_cmd_base(host_or_ip)
    return ["sudo", "-i", "-u", "root", "--", "ssh", "-o", "TCPKeepAlive=no", "-o", "ServerAliveInterval=15", "root@#{host_or_ip}"]
  end

  # get boot time of the given host or 0 if command can not be executed
  def self.ssh_cmd_get_boottime(host_or_ip)
    ssh_cmd = self.ssh_cmd_base(host_or_ip)
    ssh_cmd << "'echo $(($(date +%s) - $(cat /proc/uptime|cut -d \" \" -f 1|cut -d \".\" -f 1)))'"
    ssh_cmd = ssh_cmd.map{|x| x.chomp}.join(" ")
    retval = `#{ssh_cmd}`.split(" ")[0].to_i
    if $?.exitstatus != 0
      puts "ssh-cmd '#{ssh_cmd}' to get datetime not successful"
      retval = 0
    end
    return retval
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
