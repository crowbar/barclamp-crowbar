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

  attr_accessible :id, :description, :name, :jig_id, :barclamp_id
  attr_accessible :library, :implicit, :bootstrap, :discovery     # flags
  attr_accessible :role_template, :node_template, :min_nodes      # template info


  validates_uniqueness_of   :name,  :scope => :barclamp_id
  validates_uniqueness_of   :name,  :scope => :jig_id
  validates_format_of       :name,  :with=>/^[a-zA-Z][-_a-zA-Z0-9]*$/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")

  belongs_to      :barclamp
  belongs_to      :jig
  
  has_many        :attribs,           :dependent => :destroy

  has_many        :role_requires,     :dependent => :destroy
  alias_attribute :requires,          :role_requires

  has_many        :upstreams,         :through => :role_requires
  scope           :downstreams,       ->(r) { joins(:role_requires).where(['requires=?', r.name]) }

  def parents
    role_requires.map do |r|
      Role.find.by_name!(r.requires)
    end
  end

  def depends_on?(other)
    return false if self.id == other.id
    p = parents
    return false if p.empty?
    return true if p.any?{|i|i.id == other.id}
    p.each do |i|
      return true if i.depends_on?(other)
    end
    false
  end

  def <=>(other)
    return 0  if self.id == other.id
    return 1  if self.depends_on?(other)
    return -1 if other.depends_on?(self)
    0
  end
end
