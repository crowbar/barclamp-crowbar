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

class BarclampDependency < ActiveRecord::Base
  attr_accessible :barclamp, :prereq
  belongs_to :barclamp
  belongs_to :prereq, :class_name => "Barclamp"
end
