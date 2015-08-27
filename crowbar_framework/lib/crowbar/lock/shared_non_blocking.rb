#
# Copyright 2011-2013, Dell
# Copyright 2013-2015, SUSE LINUX GmbH
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
  class Lock::SharedNonBlocking < Lock
    # This class implements a shared (i.e. non-exclusive) lock, by
    # default on a remote node.  It is used to allow one or more
    # concurrent proposal applications to temporarily pause any
    # intervallic runs triggered by the chef-client daemon, to prevent
    # them from interfering with the orchestration of the proposals
    # being applied:
    #
    #   https://bugzilla.suse.com/show_bug.cgi?id=857375
    #
    # It has the same semantics as a shared read/write lock, except
    # that instead of readers reading, we have threads applying
    # proposals, and instead of a single writer writing, we have the
    # chef-client daemon triggering a chef-client run.

    attr_reader :node, :owner, :reason, :executor, :readers_dir

    def initialize(options = {})
      super
      %w(node owner reason).each do |option|
        unless options[option.to_sym]
          raise "#{self.class} can't be instantiated without " + \
            ":#{option} option (#{options.inspect})"
        end
        instance_variable_set "@#{option}", options[option.to_sym]
      end

      # We manage execution of the command via dependency injection,
      # so that the test suite can execute the commands locally.
      default_executor = lambda do |node, cmd, shell|
        out, err, status = RemoteNode.pipe_ssh_cmd(node, cmd, shell)
        unless status.success?
          raise self.class.exec_failure(node, cmd, shell, out, err, status)
        end
      end
      @executor = options.fetch :executor, default_executor

      # To implement shared locking semantics, we keep track of all
      # "readers" via a 'foo.lock.d/' directory which contains one
      # file per "reader".
      @readers_dir = path + ".d"

      # In order to provide a lock for the writer, we also ensure that
      # a 'foo.lock' file exists if and only if the 'foo.lock.d/'
      # directory contains one or more readers.  Doing this safely
      # requires a "meta-lock" in order to avoid races when we create
      # or remove the 'foo.lock' file.  We implement that meta-lock
      # using flock(1) on another 'foo.lock.meta' file.
      @meta_lock = path + ".meta"
    end

    def self.exec_failure(node, cmd, shell, out, err, status)
      # The shell code run inside flock(1) on the problematic node
      # has no access to the Rails Logger object, so it echoes
      # diagnostics to STDOUT / STDERR which we capture and report
      # via an exception in the Rails server log.
      msg = "On #{node}, '#{cmd}' (pid #{status.pid}) failed, exitcode #{status.exitstatus}"
      msg += "\nSTDOUT:\n" + out if out =~ /\S/
      msg += "\nSTDERR:\n" + err if err =~ /\S/
      Crowbar::Error::LockingFailure.new(msg)
    end

    def acquire
      if locked?
        logger.warn("#{owner} tried to re-acquire remote lock #{path} on #{node}")
        return
      end

      logger.debug("#{owner} acquiring remote lock #{path} on #{node}")
      shell = <<-EOSHELL.strip
        mkdir -p "#{@readers_dir}"
        file="#{@readers_dir}/#{owner}"
        if [ -e "$file" ]; then
          echo "ERROR: #{owner} already had lock on #{node} via $file" >&2
          exit 1
        fi
        echo "#{reason}" > "$file"
        echo "`date` acquired by #{owner}" >> "#{path}"
      EOSHELL
      run_with_exclusive_lock(shell)
      logger.debug("#{owner} acquired remote lock #{path} on #{node}")
      @locked = true
      self
    end

    def release
      logger.debug("#{owner} releasing remote lock #{path} on #{node}")
      shell = <<-EOSHELL.strip
        rm -f #{@readers_dir}/#{owner}
        # Only release the shared lock when there are no readers
        # left still holding it.
        if [ -d #{@readers_dir} ] && ! ls #{@readers_dir} | grep -q .; then
          rm -f #{path}
          rmdir #{@readers_dir}
        else
          echo "`date` released by #{owner}" >> "#{path}"
        fi
      EOSHELL
      run_with_exclusive_lock(shell)
      logger.debug("#{owner} released remote lock #{path} on #{node}")
      @locked = false
      self
    end

    # Use this when there are no more readers.  Mainly useful for
    # ensuring the test suite doesn't leave temporary files around.
    def clean
      raise "Can't clean active lock I hold" if locked?

      logger.debug("#{owner} cleaning remote lock #{path} on #{node}")
      shell = <<-EOSHELL.strip
        if [ -d #{@readers_dir} ] && ls #{@readers_dir} | grep -q .; then
          echo "Cannot clean active lock someone else holds" >&2
          ls -l #{@readers_dir} >&2
          exit 1
        fi
        if [ -d #{@readers_dir} ]; then
          rmdir #{@readers_dir}
        fi
        rm -f #{@meta_lock}
      EOSHELL
      run_with_exclusive_lock(shell)
    end

    private

    # Run a critical region with an exclusive lock so that no
    # concurrent thread can race with it.
    def run_with_exclusive_lock(shell)
      cmd = "flock #{@meta_lock} bash -es"
      logger.debug("Will execute on node #{node}: #{cmd}")
      @executor.call(node, cmd, shell)
    end
  end
end
