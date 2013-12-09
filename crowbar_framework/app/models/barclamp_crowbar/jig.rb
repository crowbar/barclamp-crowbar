# Copyright 2013, Dell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
#
# This model is a stub for the Jig override system
# It is NOT installed by default, but can be used for testing or as a model

require 'json'
require 'fileutils'

class BarclampCrowbar::Jig < Jig

  SSH_OPTS = [
              "-o 'UserKnownHostsFile /dev/null'", # The keys may change, so don't save them
              "-o 'StrictHostKeyChecking no'",
              "-o 'CheckHostIP no'",
              "-o 'PasswordAuthentication no'", # Always use pubkey auth.
              "-o 'ControlMaster auto'",  # Arrange to multiplex SSH connections
              "-o 'ControlPath /root/.ssh/.control-%h-%p-%r'",
              "-o 'ControlPersist 3'"  # Allow oppourtunistic master connections to live for 3 seconds.
             ]
  def self.ssh(cmd)
    out = ""
    3.times do |delay|
      out =  %x{sudo -H ssh #{SSH_OPTS.join(" ")} #{cmd}}
      return [out, $?.exitstatus == 0] if $?.exitstatus == 0 || !out.empty?
      sleep delay
    end
    return [out, false]
  end

  def self.scp(cmd)
    out = ""
    3.times do |delay|
      out =  %x{sudo -H scp #{SSH_OPTS.join(" ")} #{cmd}}
      return [out, $?.exitstatus == 0] if $?.exitstatus == 0 || !out.empty?
      sleep delay
    end
    return [out, false]
  end

  def stage_run(nr)
    return nr.all_transition_data
  end

  def run(nr,data)
    # Hardcode this for now
    login = "root@#{nr.node.name}"
    local_scripts = "/opt/dell/barclamps/#{nr.barclamp.name}/script/roles/#{nr.role.name}"
    raise "No local scripts @ #{local_scripts}" unless File.exists?(local_scripts)
    remote_tmpdir,ok = BarclampCrowbar::Jig.ssh("'#{login}' -- mktemp -d /tmp/scriptjig-XXXXXX")
    remote_tmpdir.strip!
    if remote_tmpdir.empty? || !ok
      raise "Did not create remote_tmpdir for some reason!"
    else
      Rails.logger.info("Using remote temp dir: #{remote_tmpdir}")
    end
    local_tmpdir = %x{mktemp -d /tmp/local-scriptjig-XXXXXX}.strip
    Rails.logger.info("Using local temp dir: #{local_tmpdir}")
    attr_to_shellish(data).each do |k,v|
      target = File.join(local_tmpdir,"attrs",k)
      FileUtils.mkdir_p(target)
      File.open(File.join(target,"attr"),"w") do |f|
        f.printf("%s",v.to_s)
      end
    end
    FileUtils.cp_r(local_scripts,local_tmpdir)
    FileUtils.cp('/opt/dell/barclamps/crowbar/script/runner',local_tmpdir)
    Rails.logger.info("Copying staged scriptjig information to #{nr.node.name}")
    nr.runlog,ok = BarclampCrowbar::Jig.scp("-r '#{local_tmpdir}/.' '#{login}:#{remote_tmpdir}'")
    unless ok
      Rails.logger.error("Copy failed! (status = #{$?.exitstatus})")
      nr.state = NodeRole::ERROR
      return finish_run(nr)
    end
    Rails.logger.info("Executing scripts on #{nr.node.name}")
    nr.runlog,ok = BarclampCrowbar::Jig.ssh("'#{login}' -- /bin/bash '#{remote_tmpdir}/runner' '#{remote_tmpdir}' '#{nr.role.name}'")
    unless ok
      Rails.logger.error("Script jig run for #{nr.role.name} on #{nr.node.name} failed! (status = #{$?.exitstatus})")
      nr.state = NodeRole::ERROR
      return finish_run(nr)
    end
    # Now, we need to suck any written attributes back out.
    Rails.logger.info("Retrieving any information that needs to go on the wall from #{nr.node.name}")
    new_wall = {}
    Rails.logger.info("Copying attributes from #{nr.node.name} for analysis")
    cp_log,ok = BarclampCrowbar::Jig.scp("-r '#{login}:#{remote_tmpdir}/attrs' '#{local_tmpdir}'")
    unless ok
      Rails.logger.error("Copy of attrs back from #{nr.node.name} failed! (status = #{$?.exitstatus})")
    end
    FileUtils.cd(File.join(local_tmpdir,"attrs")) do
      # All new attributes should be saved in wall files.
      Dir.glob("**/wall") do |attrib|
        k = attrib.split('/')[0..-2]
        v = IO.read(attrib).strip
        Rails.logger.info("Found attribute #{attrib} (value #{v})")
        next if v.empty?
        # Convert well-known strings and strings that look like numbers to JSON values
        v = case
            when v.downcase == "true" then true
            when v.downcase == "false" then false
            when v =~ /^[-+]?[0-9]+$/ then v.to_i
            when v =~ /^[-+]?[0'9a-fA-f]+$/ then v.to_i(16)
            when v =~ /^[-+]?0[bodx]?[0-9a-fA-F]+$/ then v.to_i(0)
            else v
            end
        w = new_wall
        # Build the appropriate hashing structure based on what were directory names.
        k[0..-2].each do |key|
          w[key] ||= Hash.new
            w = w[key]
        end
        w[k[-1]] = v
      end
    end
    Rails.logger.info("New wall values for #{nr.name} #{new_wall.inspect}")
    # By now, we have new_wall populated. Save it if anything changed.
    NodeRole.transaction do
      old_wall = nr.wall
      unless new_wall.empty? || (old_wall == new_wall)
        nr.wall = old_wall.deep_merge!(new_wall)
        nr.save!
      end
    end
    system("sudo -H chown -R crowbar.crowbar #{local_tmpdir}")
    system("sudo rm -rf '#{local_tmpdir}")
    # Clean up after ourselves.
    BarclampCrowbar::Jig.ssh("'#{login}' -- rm -rf '#{remote_tmpdir}'")
    nr.state = NodeRole::ACTIVE
    finish_run(nr)
  end

  def create_node(node)
    Rails.logger.info("ScriptJig Creating node: #{node.name}")
    # ? generate a SSH pub/private key pair
    # ? put in node: /user/root authorized_keys file
    # return JSON to be returned to the node
    {}
  end

  def delete_node(node)
    Rails.logger.info("ScriptJig Deleting node: #{node.name}")
  end

  private

  # Turn a nested hash table into an array of key/value pairs
  # This is intended to be turned into a filesystem structure that the
  # scripts being executed on the remote system can access.
  def attr_to_shellish(values, prefix=[])
    res = {}
    if values.kind_of?(Array)
      values.each_index do |i|
        res[i.to_s]=values[i]
      end
      values = res
      res = {}
    end
    values.each do |k,v|
      key = prefix.dup << k.to_s
      case
      when v.nil? then next
      when v.kind_of?(Hash) && !v.empty?
        res.merge!(attr_to_shellish(v,key)) unless v.empty?
      when v.respond_to?(:to_s) then res[key.join("/")] = v.to_s
      end
    end
    res
  end

end
