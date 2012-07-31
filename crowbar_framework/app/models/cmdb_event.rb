class CmdbEvent < ActiveRecord::Base
  belongs_to :cmdb_run
  attr_accessible :attributes, :direction, :name, :result, :status, :cmdb_run, :type
end
