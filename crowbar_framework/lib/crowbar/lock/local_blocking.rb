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
  class Lock::LocalBlocking < Lock
    def acquire
      logger.debug("Acquire #{name} lock enter as uid #{Process.uid}")
      begin
        @file ||= File.new(path, File::RDWR | File::CREAT, 0644)
      rescue
        logger.error("Couldn't open #{path} for locking: #$!")
        logger.error("cwd was #{Dir.getwd})")
        raise "Couldn't open #{path} for locking: #$!"
      end
      logger.debug("Acquiring #{name} lock")
      count = 0
      loop do
        count += 1
        logger.debug("Lock #{path} attempt #{count}")
        if file.flock(File::LOCK_EX | File::LOCK_NB)
          break
        end
        sleep 1
      end
      logger.debug("Acquire #{name} lock exit: #{file.inspect}")
      @locked = true
      self
    end

    def release
      logger.debug("Release #{name} lock enter: #{file.inspect}")
      if @file
        @file.flock(File::LOCK_UN) if locked?
        @file.close unless @file.closed?
        @file = nil
      else
        logger.warn("release called without valid file")
      end
      logger.debug("Release #{name} lock exit")
      @locked = false
      self
    end
  end
end
