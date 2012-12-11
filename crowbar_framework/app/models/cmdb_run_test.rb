# Copyright 2012, Dell
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

#
# This model is a stub for the CMDB override system
# It is NOT installed by default, but can be used for testing or as a model

class CmdbRunTest < CmdbRun

  def init
    super.init
  end

  def run_cmdb_on_node(node)
    super.run_cmdb_on_node node
  end

  # map node from Chef into an array of CmdbAttributes
  def attrs_from_cmdb(cmdb, node)
    super.attrs_from_cmdb
  end
  
  def attrs_to_cmdb(cmdb, node)
    super.attrs_to_cmdb(cmdb, node)
  end

  # make sure I can get the map I need to put attrs in the DB
  def map(map_id)
    super.map
  end

end
