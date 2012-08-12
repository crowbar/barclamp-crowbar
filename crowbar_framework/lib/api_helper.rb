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
# This class AUTOMATICALLY extends the ActiveRecord base class 
# so that we can add AR helpers for Crowbar
module ApiHelper
#/lib/api_helper.rb

  def self.included(base)
    base.extend(ClassMethods)
  end

  module ClassMethods
    
    # Helper that returns SET of all (or limited listed based on ID or name)
    def find_keys (key)
      r = []
      if key.nil?
        r = all
      else 
        r << find_key(key)
      end
      return r
    end
  
    # Helper to allow API to use ID or name
    def find_key (key)
      if key.is_a? Integer or key =~ /^[0-9]/
        find(key.to_i)
      else
        find_by_name(key)
      end
    end

  end 

end
ActiveRecord::Base.send :include, ApiHelper
