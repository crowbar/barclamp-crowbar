# Copyright 2011, Dell
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
# Author: RobHirschfeld
#
class Doc < ActiveRecord::Base
  
  self.primary_key = "name"  
  attr_accessible :name, :parent_name, :description, :author, :license, :copyright, :date, :order

  belongs_to :parent, :class_name => "Doc", :foreign_key => "parent_name"
  has_many :children, :class_name => "Doc", :foreign_key => "parent_name", :order => "[order]+[description] ASC"

  validates_uniqueness_of :name, :on => :create, :message => I18n.t("db.notunique", :default=>"Doc handle must be unique")
  
end