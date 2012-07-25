#############
# Node_role is an association class between a role in a proposal configuration and the
# node that is assigned that role. This supports a many2many association between
# roles and nodes, with some extra info.

class NodeRole < ActiveRecord::Base
  attr_accessible :role_name, :config, :status
  has_many        :node
  belongs_to      :role
end
