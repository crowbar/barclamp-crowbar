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

# Adds the attributes that we want to generically expose for Nodes
class AddNodeAttribs < ActiveRecord::Migration

  def self.up

    # note, these descriptions are NOT localized, we may have to consider that in the future

    Attrib.create :name=>'asset_tag', :description=>'BIOS configured system identifier', :map=>'ohai/dmi/base_board/asset_tag'
    Attrib.create :name=>'serial_number', :description=>'System Serial Number', :map=>'ohai/dmi/chassis/serial_number'

    Attrib.create :name=>'kernel', :description=>'Kermel Name', :map=>'ohai/kernel/name'
    Attrib.create :name=>'kernel_version', :description=>'Kernel Version', :map=>'ohai/kernel/version'

    Attrib.create :name=>'os', :description=>'O/S Name', :map=>'ohai/platform'
    Attrib.create :name=>'os_version', :description=>'O/S Version', :map=>'ohai/platform_version'
    Attrib.create :name=>'os_description', :description=>'O/S Description', :map=>'ohai/lsb/description'

    Attrib.create :name=>'memory', :description=>'System Memory', :map=>'ohai/memory/total'
    Attrib.create :name=>'cpu', :description=>'System Processor', :map=>'ohai/cpu/0/model_name'
    Attrib.create :name=>'cpu_count', :description=>'Number of Processors', :map=>'ohai/cpu/total'
    Attrib.create :name=>'hardware', :description=>'Product Name', :map=>'ohai/dmi/system/product_name'
    Attrib.create :name=>'manufacturer', :description=>'Product Vendor', :map=>'ohai/dmi/system/manufacturer'

    BarclampCrowbar::AttribDriveCount.create :name=>'number_of_drives', :description=>'RAID set', :map=>'ohai/block_device'

# Rob's Attribute list still TBD
    # we have not done any lists yet
        #physical_drives (list) 
        #nics (list) -

    # consider putting in other barclamps
        #raid_set -> should be in RAID barclamp
        #bios_set -> ["crowbar"]["hardware"]["bios_set"] 
        #get_bmc_user -> ["ipmi"]["bmc_user"] 
        #get_bmc_password-> ["ipmi"]["bmc_password"] 
        #bmc_address

    #switch name, mac, port, unit

  end

  def self.down
    keys = ['asset_tag', 'serial_number', 'kernel', 'kernel_version', 'mac_address',
             'os', 'os_version', 'os_description', 'memory', 'cpu', 
              'cpu_count', 'hardware', 'manufacturer']
    keys.each { |k| Attrib.delete Attrib.find_key(k).id }
  end
end
