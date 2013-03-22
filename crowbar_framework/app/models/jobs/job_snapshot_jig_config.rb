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
# Author: aabes

#####
# this class is Jig type agnostic - it collates the config info for a snapshot,
# but is inteded to be subclassed by Jig specific 

class Jobs::SnapshotJigConfig < Jobs::BaseJob
  
  attr_accessor :snapshot
  attr_accessible :snapshot
  serialize :data, JSON  #expecting to only save ID's of things...

  # store the ID's of the refrenced objects when creating a new record.
  before_save :save_data
  after_find  :read_data

  def save_data    
    self.data = { "snapshot_id" => self.snapshot.id } if new_record?
  end

  # if loading from the DB, restore the actual model object pointers on load.
  def read_data
    sid = data["snapshot_id"]
    self.snapshot = Snapshot.find_by_id sid
    raise "unknown snapshot #{sid}" if self.snapshot.nil?
  end

  # snapshot config, are those attributes of a snapshot that are not specific to a node
  # but are tied to some of the roles of the snapshot
  def find_config 
    config_attribs = []
    snapshot.roles.each { |r|
      rattribs = r.role_attribs
      next if rattribs.nil? or rattribs.length ==0
      rattribs.each { |x| config_attribs << x }
    }
    config_attribs
  end

  def perform
    Rails.logger.warn "DEBUG: Jobs was tested"
  end

end


