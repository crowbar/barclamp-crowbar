class CmdbMap < ActiveRecord::Base
  belongs_to :barclamp
  attr_accessible :name, :version
  has_many :cmdb_attributes
end
