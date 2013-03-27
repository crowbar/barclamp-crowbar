
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
class BarclampImportCrowbar < ActiveRecord::Migration
  def up
    Barclamp.import_1x 'crowbar'
    # this is needed for testing
    if Rails.env.development? or Rails.env.test? 
      Role.create :name=>'crowbar', :snapshot_id=>Barclamp.find_by_name('crowbar').template_id, :description=>'DEVELOPER - added for dev and test.  May cause issues'
    end
  end

  def down
    Barclamp.delete(Barclamp.find_by_name 'crowbar')
  end
  
end
