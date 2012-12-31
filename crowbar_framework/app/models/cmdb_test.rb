# Copyright 2012, Dell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
#
# This model is a stub for the CMDB override system
# It is NOT installed by default, but can be used for testing or as a model
class CmdbTest < Cmdb

  def init
    super.init
  end

  def create_event(config)
    puts "$$$ #{Time.now.to_i} #{id}"
    CmdbEvent.create(:name=>Time.now.to_i.to_s, :cmdb_id => id, :type=>'CmdbEventTest', :status => CmdbEvent::EVT_PENDING)
  end
  
  def run(config_id)
    super.run config_id
    config_id
  end

  def delete_node(node)
    name = node.name
    Rails.logger.info("Test CMDB #{self.name} is removing the node #{name} from the system")
  end

  def node(name)
    # override
    super.node name
    name
  end

  def data(key)
    super.data key
    key
  end

end


