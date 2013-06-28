# Copyright 2013, Dell 
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


#
# Helper functions
# 
class CrowbarUtils

  @@lock_scoreboard=Hash.new

  #
  # Locking Routines
  #
  private

  def self.try_lock(name)
    f = File.new("tmp/#{name}.lock", File::RDWR|File::CREAT, 0644)
    raise IOError.new("File not available: tmp/#{name}.lock") unless f
    if f.flock(File::LOCK_EX|File::LOCK_NB)
      return f
    end
    f.close
    return false
  end

  public

  def self.lock_held?(name)
    if f = try_lock(name)
      f.flock(File::LOCK_UN)
      f.close
      return false
    end
    true
  end

  def self.with_lock(name,*args)
    f = nil
    spin = 30
    raise "CrowbarUtils.with_lock must be passed a block!" unless block_given?
    Rails.logger.debug("CrowbarUtils.with_lock: Acquiring lock #{name}")
    while ! (f = try_lock(name)) && (spin >= 0)
      Rails.logger.debug("#{spin} tries left to grab #{name} lock")
      spin = spin - 1
      sleep 1
    end
    unless f
      e = RuntimeError.new
      mesg = []
      if @@lock_scoreboard[name]
        mesg << "Call trace of last code to grab the lock:"
        mesg = mesg + @@lock_scoreboard[name]
        mesg << "Call trace of last holder finished."
      end
      mesg << "Call trace of current process:"
      e.set_backtrace(mesg + caller())
      raise(e,"Unable to grab #{name} lock -- Probable deadlock.")
    end
    Rails.logger.debug("CrowbarUtils.with_lock: Acquired lock #{name}")
    @@lock_scoreboard[name]=caller()
    begin
      yield(*args)
    ensure
      @@lock_scoreboard.delete(name)
      f.flock(File::LOCK_UN)
      f.close
      Rails.logger.debug("CrowbarUtils.with_lock: Released lock #{name}")
    end
  end
end
