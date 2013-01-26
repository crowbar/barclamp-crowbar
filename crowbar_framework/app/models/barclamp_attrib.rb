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

class BarclampAttrib < ActiveRecord::Base

  BARCLAMP_ID_SPACE = 100000
  BARCLAMP_NAME_DELIM = '@'
  
  before_create :create_identity

  attr_accessible :barclamp_id, :attrib_id, :description, :order
  attr_readonly   :name

  belongs_to  :attrib
  belongs_to  :barclamp

  self.primary_key = 'generated_id'

  alias_attribute :map, :description
  
  def self.find(id)
    BarclampAttrib.find_by_generated_id id
  end

  def self.delete_by_barclamp_and_attrib(barclamp, attrib)
    ba = BarclampAttrib.find BarclampAttrib.id_generate(barclamp.id, attrib.id)
    if ba.nil?
      id = -1
    else
      id = ba.id
      id = (ba.delete ? id : -1 )
    end
    id
  end

  def self.find_by_barclamp_and_attrib(barclamp, attrib)
    throw "Barclamp provided cannot be nil" unless barclamp
    throw "Attrib provided cannot be nil" unless attrib
    BarclampAttrib.find BarclampAttrib.id_generate(barclamp.id, attrib.id)
  end
    
  def self.find_or_create_by_barclamp_and_attrib(barclamp, attrib)
    na = find_by_barclamp_and_attrib barclamp, attrib
    na = BarclampAttrib.create(:barclamp_id=>barclamp.id, :attrib_id=>attrib.id) if na.nil?
    na
  end

  def self.name_generate barclamp, attribute
    "#{attribute.name}#{BARCLAMP_NAME_DELIM}#{barclamp.name}"
  end
  
  def self.id_generate barclamp, attribute
    barclamp*BARCLAMP_ID_SPACE+attribute
  end

  def id
    return self.generated_id
  end
  
  def as_json options={}
   {
     :id=> generated_id,
     :barclamp_id=> barclamp_id,
     :attrib_id=> attrib_id,
     :name=> name,
     :order => order,   
     :description=> description, 
     :created_at=> created_at,
     :updated_at=> updated_at
   }
  end
  
  private
  
  # make sure some safe values are set for the barclamp
  def create_identity
    throw "BarclampAttrib cannot create without a Barclamp ID" unless self.barclamp_id
    b = Barclamp.find self.barclamp_id
    throw "BarclampAttrib cannot create without a valid Barclamp (ID was #{self.barclamp_id})" unless b
    throw "BarclampAttrib cannot create without an Attrib ID" unless self.attrib_id
    a = Attrib.find self.attrib_id
    throw "BarclampAttrib cannot create without a valid Attrib (ID was #{self.attrib_id})" unless a
    self.generated_id = BarclampAttrib.id_generate b.id, a.id
    self.name = BarclampAttrib.name_generate b, a
  end
  
end
