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
class AttribType < ActiveRecord::Base

    
  attr_accessible :name, :description, :order

  validates_format_of     :name, :with=>/^[a-zA-Z][_a-zA-Z0-9]*$/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")
  validates_uniqueness_of :name, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")

  has_many    :attribs,             :dependent => :destroy
  has_many    :nodes,               :through => :attribs
  
  has_many    :roles,               :through => :attribs, :order=>"'order', 'role_order'"
  has_many    :snapshots,           :through => :roles
  
  has_many    :jig_maps,            :dependent => :destroy 
  alias_attribute :maps,            :jig_maps

  has_many    :jigs,                :through => :jig_maps
  has_many    :barclamps,           :through => :jig_maps
   
  def self.add(attrib_type, source='unknown')
    # this should become a switch as some point!
    if attrib_type.nil?       
      raise "attrib_type.add requires Attrib object or hash with :name"
    elsif attrib_type.is_a? AttribType
      a = attrib_type
    elsif attrib_type.is_a? Attrib
      a = attrib_type.attrib_type
    elsif attrib_type.is_a? Fixnum
      a = AttribType.find attrib_type
    elsif attrib_type.is_a? String or attrib_type.is_a? Symbol
      # we can make them from just a string
      a = AttribType.find_or_create_by_name :name => attrib_type.to_s, :description => I18n.t('model.attribs.barclamp.default_create_description', :barclamp=>source)
      a.save!
    elsif attrib_type.is_a? Hash
      # we can make them from a hash if the creator wants to include more info
      raise "attrib_type.add requires attribute :name" if attrib_type.nil? or !attrib_type.has_key? :name
      a = AttribType.find_or_create_by_name attrib_type
      a.save!
    else
      raise "attrib_type.add cannot use #{attrib_type.class} to create from attribute: #{attrib_type.inspect}"
    end 
    Rails.logger.info("attrib add returning #{a.inspect}")
    a
  end
  
end

