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
class CrowbarBarclampImport < ActiveRecord::Migration
  def up
    import_1_0 'crowbar'
  end

  def down
    Barclamp.delete(Barclamp.find_by_name 'crowbar')
  end
  
  def import_1_0(barclamp)
    bc = YAML.load_file File.join('barclamps', barclamp+'.yml')
    throw 'Barclamp name must match name from YML file' unless bc['barclamp']['name'].eql? barclamp
    Barclamp.create_by_name(barclamp) do |b|
        b.display     = bc['barclamp']['display'] || barclamp.titlize
        b.description = bc['barclamp']['description'] || barclamp.titlize
        b.online_help = bc['barclamp']['online_help']
        b.version     = bc['barclamp']['version'] || 2
        b.proposal_schema_version = bc['barclamp']['proposal_schema_version'] || 2
        bc['barclamp']['member'].each do |m|
          b.member << m
        end
    end
  end
end
