#
# Copyright 2015, SUSE LINUX GmbH
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

module Crowbar
  class Lock
    attr_accessor :logger
    attr_accessor :path
    attr_accessor :remote
    attr_accessor :locked
    attr_accessor :local_file

    def initialize(options = {})
      @logger = options.fetch :logger, Rails.logger
      @path = options.fetch :path, Rails.root.join("tmp", "default.lock") # full path to lockfile
      @remote = options.fetch :remote, false # or nodes array
      @locked = false
      @local_file = nil
    end

    def with_lock
      acquire
      yield if block_given?
    ensure
      release
    end

    def acquire
      if remote
        remote_lock(action: "acquire")
      else
        logger.debug("Acquire #{path} lock enter as uid #{Process.uid}")
        begin
          self.local_file = File.new(path, File::RDWR | File::CREAT, 0644)
        rescue
          logger.error("Couldn't open #{path} for locking: #$!")
          logger.error("cwd was #{Dir.getwd})")
          raise "Couldn't open #{path} for locking: #$!"
        end
        logger.debug("Acquiring #{path} lock")
        rc = false
        count = 0
        while rc == false do
          count = count + 1
          logger.debug("Attempt #{path} Lock: #{count}")
          rc = local_file.flock(File::LOCK_EX | File::LOCK_NB)
          sleep 1 if rc == false
        end
        logger.debug("Acquire #{path} lock exit: #{local_file.inspect}, #{rc}")
      end
      self.locked = true
      self
    end

    def release
      return self unless locked
      if remote
        remote_lock(action: "release")
      else
        logger.debug("Release lock enter: #{local_file.inspect}")
        if local_file
          local_file.flock(File::LOCK_UN)
          local_file.close
        else
          logger.warn("release_lock called without valid file")
        end
        logger.debug("Release lock exit")
      end
      self.locked = false
      self
    end

    protected

    def remote_lock(options = {})
      action = options.fetch :action
      remote.each do |node|
        if action == "acquire"
          @logger.info("Creating remote temporary lockfile #{path} on #{node}")
          ssh_cmd(node, "touch")
        elsif action == "release"
          ssh_cmd(node, "rm -f")
        end
      end
    end

    def ssh_cmd(node, cmd)
      RemoteNode.ssh_cmd(node, "#{cmd} #{path}")
    end
  end
end
