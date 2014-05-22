# -*- encoding : utf-8 -*-
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

class FileLock
  def self.acquire(name, options = {})
    logger = options.fetch(:logger) { self.logger }
    logger.debug("Acquire #{name} lock enter as uid #{Process.uid}")

    path = "tmp/#{name}.lock"

    begin
      f = File.new(path, File::RDWR|File::CREAT, 0644)
    rescue
      logger.error("Couldn't open #{path} for locking: #$!")
      logger.error("cwd was #{Dir.getwd})")

      raise "Couldn't open #{path} for locking: #$!"
    end

    logger.debug("Acquiring #{name} lock")
    rc = false
    count = 0

    while rc == false do
      count = count + 1
      logger.debug("Attempt #{name} Lock: #{count}")
      rc = f.flock(File::LOCK_EX | File::LOCK_NB)
      sleep 1 if rc == false
    end

    logger.debug("Acquire #{name} lock exit: #{f.inspect}, #{rc}")

    f
  end

  def self.release(f, options = {})
    logger = options.fetch(:logger) { self.logger }
    logger.debug("Release lock enter: #{f.inspect}")

    if f
      f.flock(File::LOCK_UN)
      f.close
    else
      logger.warn("release_lock called without valid file")
    end

    logger.debug("Release lock exit")
  end

  def self.logger
    defined?(Rails) ? Rails.logger : Logger.new(STDERR)
  end
end
