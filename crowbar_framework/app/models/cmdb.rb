class Cmdb < ActiveRecord::Base
  attr_accessible :name
  
  has_many :cmdb_runs

end
