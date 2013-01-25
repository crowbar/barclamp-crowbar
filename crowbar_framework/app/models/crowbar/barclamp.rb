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

# This class is the fall back class for barclamps that are missing Barclamp subclasses
class Crowbar::Barclamp < Barclamp
  
  def init
    super.init

    # register attributes of interest 
    self.add_attrib "physical_drives"
    self.add_attrib "memory"
    self.add_attrib "cpu_type"
    self.add_attrib "cpu_count"
    self.add_attrib "dmi_product_name"
    self.add_attrib "raid_set"
    self.add_attrib "nics_count"
    self.add_attrib "nics_list"
    self.add_attrib "uptime"
    self.add_attrib "drives_count"
    self.add_attrib "drives_list"
    # DMI data collected by Crowbar 
    self.add_attrib "product_info",   "dmi/system/product_info"
    self.add_attrib "node_family",    "dmi/system/family"
    self.add_attrib "manufacturer",   "dmi/system/manufacturer"
    self.add_attrib "product_name",   "dmi/system/product_name"
    self.add_attrib "serial_number",  "dmi/system/serial_number"
    self.add_attrib "node_uuid",      "dmi/system/uuid"
              
    # THESE SHOULD BE MOVED INTO THE BARCLAMPS THE SUPPLY THEM!!! 

    # likely move to network barclamp
    self.add_attrib "public_ip"
    self.add_attrib "mac"                       #eth0
    # likely move to DNS barclamp
    self.add_attrib "alias", "dns/alias"
    # likely move to bios barclamp
    self.add_attrib "asset_tag"
    self.add_attrib "bios_set", "crowbar/hardware/bios_set"
    # likely move to impi barclamp
    self.add_attrib "impi_enabled"
    self.add_attrib "bmc_user", "ipmi/bmc_user" 
    self.add_attrib "bmc_password", "ipmi/bmc_password"
    self.add_attrib "bmc_address", "ipmi/bmc_address"
    
  end
  
end
