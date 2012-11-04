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
class CmdbMap < ActiveRecord::Base
  attr_accessible :name, :description, :order, :map, :revision
  
  belongs_to :barclamp

  has_many :cmdb_runs
  has_many :cmdb_attributes

  attr_writer :cmdb_backend

  def backend
    @backend || 'chef'
  end

  def mapping_hash
    {} unless map
    JSON::parse(map)
  end
  
  # unnecessary?  probably.
  def mapping_hash=(mhash)
    map = mhash.to_json
    save!
  end
  
  # rather than just returning an array, I'd like this to be # an enumerable sorta thing 
  # but I'm not there yet with my ruby
  def mapping
    new_mapping = {}
    mapping_hash.each do | attribute, sub_pair |
      next unless cmdb_attr = sub_pair.fetch(backend)
      new_mapping[attribute] = cmdb_attr
    end
    return new_mapping
  end

end
