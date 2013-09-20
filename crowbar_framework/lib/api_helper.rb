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
# This class AUTOMATICALLY extends the ActiveRecord base class 
# so that we can add AR helpers for Crowbar
module ApiHelper
#/lib/api_helper.rb

  def self.included(base)
    base.extend(ClassMethods)
    base.extend(InstanceMethods)
  end

  # for the top level classes (finders, etc)
  module ClassMethods
    
    # Helper that returns SET of all (or limited listed based on ID or name)
    def find_keys(key)
      begin 
        if key.nil?
          all
        elsif db_id?(key)
          find_all_by_id key.to_i
        else key.is_a? String
          find_all_by_name key
        end
      rescue ActiveRecord::RecordNotFound => e
        []
      end
    end
  
    # Helper to allow API to use ID or name
    def find_key(key)
      begin
        if db_id?(key)
          find key.to_i
        elsif key.is_a? String
          if key =~ /^[0-9]+$/
            find key.to_i
          else
            find_by_name key rescue nil
          end
        elsif key.is_a? ActiveRecord::Base  
          # if we get the object itself then use find to valid it exists
          find key.id
        end          
      rescue ActiveRecord::RecordNotFound => e
        nil
      end
    end

    # get id of object by id string or name string
    def find_real_key(key)
      begin
        if db_id?(key)
          key.to_i
        elsif key.is_a? String
          o = find_by_name key
          o.id || nil
        end
      rescue ActiveRecord::RecordNotFound => e
        nil
      end
    end

    # Helper to determine if a given key is an ActiveRecord DB ID
    def db_id?(key)
      key.is_a? Fixnum or key.is_a? Integer or key =~ /^[0-9]+$/
    end
  end 

  # for each instance (so we can use self)
  module InstanceMethods

  end

end
ActiveRecord::Base.send :include, ApiHelper
