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

class CmdbEvent < ActiveRecord::Base
  attr_accessible :attributes, :direction, :name, :result, :status, :cmdb_run, :type

  belongs_to :cmdb_run
  #belongs_to :node  # through cmdb_run
  #belongs_to :cmdb  # through cmdb_run

end
