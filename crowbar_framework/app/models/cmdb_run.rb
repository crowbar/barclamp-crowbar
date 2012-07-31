class CmdbRun < ActiveRecord::Base
  attr_accessible :title, :body

  belongs_to :cmdb

  has_many :cmdb_events
end
