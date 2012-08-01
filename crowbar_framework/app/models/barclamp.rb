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

class Barclamp < ActiveRecord::Base
  attr_accessible :description, :version, :group, :name
  has_many :proposals
  ## dependnecies are tracked using an explict join-table, barclamp_dependncies
  ## to add a dependency, create one of those, setting prereq to the depend
  # A quick way to achieve that is (b1,b4 are barclamp instances)
  # b1.barclamp_dependencies << BarclampDependency.create( { :barclamp =>b1, :prereq =>b4} )

  has_many :barclamp_dependencies, :inverse_of => :barclamp
  has_many :prereqs, :class_name => "Barclamp", :through => :barclamp_dependencies
end

