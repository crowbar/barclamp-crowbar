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

class Role < ActiveRecord::Base

  # priority: the order this role gets applied in, system wide
  # states: node states that this role will be included in the excution list for the node
  attr_accessible :name, :states, :order, :description
  
  validates_format_of :name, :with=>/^[a-zA-Z][_a-zA-Z0-9]*$/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")
  validates_uniqueness_of :name, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")

  # an element_order determines the execution group(s) of the role, relative to other roles in 
  # the barcalmp.
  has_many :role_element_orders,  :dependent => :destroy 
  
  has_many :role_instances,       :dependent => :destroy, :inverse_of => :role

  alias_attribute :priority,      :order
  
  def self.find_private  
    self.find_or_create_by_name :name => "private", 
                                :description => I18n.t('model.role.private_role_description'),
                                :order => 1
  end
  
  def self.add(role, source='unknown')
    # this should become a switch as some point!
    if role.is_a? Role
      r = role
    elsif role.is_a? RoleInstance
      r = role.role
    elsif role.is_a? String or role.is_a? Symbol
      # we can make them from just a string
      desc = I18n.t 'model.role.default_create_description', :name=>source
      r = Role.find_or_create_by_name :name => role.to_s, :description => desc
    elsif role.is_a? Hash
      # we can make them from a hash if the creator wants to include more info
      throw "role.add requires attribute :name" if role.nil? or !role.has_key? :name
      r = Role.find_or_create_by_name role
    else
      throw "role.add cannot use #{role.class || 'nil'} to create from attribute: #{role.inspect}"
    end 
    r
  end
  
end

