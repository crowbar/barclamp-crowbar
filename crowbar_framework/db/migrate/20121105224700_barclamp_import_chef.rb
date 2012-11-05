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

# TODO - this belongs in it's own barclamp!!!
class BarclampImportChef < ActiveRecord::Migration
  def up
    Cmdb.find_or_create_by_name!(:name=>'chef', :descripton=>'Opscode Chef', :type=>'CmdbChef', :order=>1000)
  end

  def down
    Cmdb.delete(Cmdb.find_by_name 'chef')
  end
  
end
