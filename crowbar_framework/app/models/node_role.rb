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

class NodeRole < ActiveRecord::Base

  attr_accessible :state, :status, :data, :wall
  attr_accessible :role_id, :snapshot_id, :node_id

  has_one :node
  has_one :snapshot
  has_one :role

  ERROR     = -1
  ACTIVE    = 0
  TODO      = 1
  COMMITTED = 2
  PROPOSED  = nil

  def error?
    state < 0
  end

  def active?
    state == 0
  end

  def todo?
    state == 1
  end

  def committed?
    state > 1
  end

  def proposed?
    state.nil?
  end
  
  # helpers if you don't want to deal w/ raw data
  def value=(v)
     data = ActiveSupport::JSON.encode(v)
  end

  # helpers if you don't want to deal w/ raw data
  def value
    if data.eql? MARSHAL_EMPTY
      nil
    else
      ActiveSupport::JSON.decode(data)
    end
  end
  

end
