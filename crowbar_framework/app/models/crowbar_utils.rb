# Copyright 2012, Dell 
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

  #
  # Locking Routines
  #
  def self.acquire_lock(name)
    Rails.logger.debug("Acquire #{name} lock enter")
    f = File.new("tmp/#{name}.lock", File::RDWR|File::CREAT, 0644)
    raise IOError.new("File not available: tmp/#{name}.lock") unless f
    Rails.logger.debug("Acquiring #{name} lock")
    rc = false
    count = 0
    while rc == false do
      count = count + 1
      Rails.logger.debug("Attempt #{name} Lock: #{count}")
      rc = f.flock(File::LOCK_EX|File::LOCK_NB)
      sleep 1 if rc == false
    end
    Rails.logger.debug("Acquire #{name} lock exit: #{f.inspect}, #{rc}")
    f
  end

  def self.release_lock(f)
    raise IOError.new("Invalid file") unless f
    Rails.logger.debug("Release lock enter: #{f.inspect}")
    f.flock(File::LOCK_UN)
    f.close
    Rails.logger.debug("Release lock exit")
  end

end
