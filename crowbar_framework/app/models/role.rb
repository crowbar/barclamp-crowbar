class Role < ActiveRecord::Base
  attr_accessible :name
  belongs_to :barclamp
end

class NodeRole < ActiveRecord::Base
  belongs_to :node
  belongs_to :role
end
