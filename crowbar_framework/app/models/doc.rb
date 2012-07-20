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
class Docs < ActiveRecord

  has_one :parent, :class_name => "docs", :foreign_key => "parent_handle"
  has_many :children, :class_name => "docs", :foreign_key => "parent_handle"
  
  validates_uniqueness_of :handle, :on => :create, :message => I18n.t("db.notunique", :default=>"Doc handle must be unique")

  def next
    parent.children.last
  end
  
  def prev
    parent.children.first
  end

  def parent_handle
    s = handle.split '+'
    s.remove[s.length]
    s.join['+']
  end

  # how deep is this below the root?
  def level
    s = handle.split '+'
    s.length-1
  end
  
end