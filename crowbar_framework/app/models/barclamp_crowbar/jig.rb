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
  def run(nr)
    raise "Cannot call ScriptJig::Run on #{nr.name}" unless nr.state == NodeRole::TRANSITION
    sshopts = SSH_OPTS.join(" ")
    Rails.logger.info("Using SSH opts: #{sshopts}")
    # Hardcode this for now
    login = "root@#{nr.node.name}"
    local_scripts = "/opt/dell/barclamps/#{nr.barclamp.name}/script/roles/#{nr.role.name}"
    raise "No local scripts @ #{local_scripts}" unless File.exists?(local_scripts)
    remote_tmpdir = %x{sudo -H ssh #{sshopts} #{login} -- mktemp -d /tmp/scriptjig-XXXXXX}.strip
    if remote_tmpdir.empty? || $?.exitstatus != 0
      raise "Did not create remote_tmpdir for some reason!"
    else
      Rails.logger.info("Using remote temp dir: #{remote_tmpdir}")
    end
    Dir.glob(File.join(local_scripts,"*.sh")).sort.each do |scriptpath|
      script = scriptpath.split("/")[-1]
      remote_script = "#{remote_tmpdir}/#{script}"
      Rails.logger.info("Copying #{scriptpath} to #{remote_script} on #{nr.node.name}")
      cp_log = %x{sudo -H scp #{sshopts} #{scriptpath} #{login}:#{remote_script}}
      if $?.exitstatus != 0
        Rails.logger.error("Copy of #{scriptpath} failed! (status = #{$?.exitstatus})")
        Rails.logger.error("Output of copy process:")
        Rails.logger.error(cp_log)
        Rails.logger.error("End of output")
        nr.state = NodeRole::ERROR
        return nr
      end
      Rails.logger.info("Executing #{remote_script} on #{nr.node.name}")
      run_log = %x{sudo -H ssh #{sshopts} #{login} -- /bin/bash #{remote_script}}
      if $?.exitstatus != 0
        Rails.logger.error("Execution of #{remote_script} on #{nr.node.name} failed! (status = #{$?.exitstatus})")
        Rails.logger.error("Output from remote execution:")
        Rails.logger.error(run_log)
        Rails.logger.error("End of output")
        nr.state = NodeRole::ERROR
        return nr
      else
        Rails.logger.error("Output from remote execution of #{remote_script}")
        Rails.logger.error(run_log)
        Rails.logger.error("End of output")
      end
    end
    nr.state = NodeRole::ACTIVE
    return nr
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

end
