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
class Attrib < ActiveRecord::Base

    
  attr_accessible :name, :description, :order

  validates_format_of     :name, :with=>/^[a-zA-Z][_a-zA-Z0-9]*$/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")
  validates_uniqueness_of :name, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")

  has_many    :attrib_instances,    :dependent => :destroy
  has_many    :nodes,               :through => :attrib_instances
  
  has_many    :role_instances,      :through => :attrib_instances, :order=>"'order', 'role_order'"
  has_many    :barclamp_instances,  :through => :role_instances
  alias_attribute :instances,       :barclamp_instances
  
  has_many    :jig_maps,            :dependent => :destroy 
  has_many    :jigs,                :through => :jig_maps
  has_many    :barclamps,           :through => :jig_maps
   
  def self.add(attrib, source='unknown')
    # this should become a switch as some point!
    if attrib.nil?       
      throw "barclamp.add_attrib requires Attrib object or hash with :name"
    elsif attrib.is_a? Attrib
      a = attrib
    elsif attrib.is_a? AttribInstance
      a = attrib.attrib
    elsif attrib.is_a? Fixnum
      a = Attrib.find attrib
    elsif attrib.is_a? String or attrib.is_a? Symbol
      # we can make them from just a string
      a = Attrib.find_or_create_by_name :name => attrib.to_s, :description => I18n.t('model.attribs.barclamp.default_create_description', :barclamp=>source)
    elsif attrib.is_a? Hash
      # we can make them from a hash if the creator wants to include more info
      throw "barclamp.add_attrib requires attribute :name" if attrib.nil? or !attrib.has_key? :name
      a = Attrib.find_or_create_by_name attrib
    else
      throw "barclamp.add_attrib cannot use #{attrib.class} to create from attribute: #{attrib.inspect}"
    end 
    a
  end
  
end

