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
class JigMap < ActiveRecord::Base
  
  attr_accessible :map
  attr_accessible :jig_id, :barclamp_id, :attrib_type_id
  
  belongs_to :jig
  belongs_to :barclamp
  belongs_to :attrib_type

  DEFAULT_JIG = :chef

  def self.get_map(jig, barclamp, attrib_type)
    j = Jig.find_by_name jig
    b = Barclamp.find_by_name barclamp
    a = AttribType.find_by_name attrib_type
    JigMap.find_by_jig_id_and_barclamp_id_and_attrib_type_id j.id, b.id, a.id
  end

  # adds the map relation between the attrib and barclamp for each jig
  def self.add(attrib_type, barclamp, map)
    maps = []
    # be super friendly for chef and convert into the hash anyway assuming they wanted chef
    map = {DEFAULT_JIG=>map} if map.is_a? String
    # we want to add a test jig too
    map[:crowbar] = map[DEFAULT_JIG] unless Rails.env.production?
    # map into jig 
    map.each do |jig, path|
      # we will keep data that passed even if the jig is not created yet!
      jtype = "Barclamp#{jig.to_s.camelize}::Jig"
      jigs = Jig.find_all_by_type jtype
      begin
        if jigs.empty?
          desc = I18n.t 'map_add', :scope=>'model.barclamp', :name=>barclamp.name
          jigs << Jig.create(:name=>jig, :type => jtype, :description => desc) rescue nil
        end
        jigs.each do |j|
           maps << JigMap.find_or_create_by_attrib_type_id_and_barclamp_id_and_jig_id(
              :attrib_type_id => attrib_type.id, 
              :barclamp_id => barclamp.id, 
              :jig_id => j.id, 
              :map => path) if j
        end
      rescue
        Rails.logger.debug "JigMap.add failed to create map between #{attrib_type.name}+#{barclamp.name}+#{jig} with path #{path}"
      end
    end  
    maps
  end
  
end
