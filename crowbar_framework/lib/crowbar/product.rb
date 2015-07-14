#
# Copyright 2015, SUSE LINUX Products GmbH
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

module Crowbar
  class Product
    def self.is_ses?
      # This checks if the product is SUSE Enterprise Storage.  It's not
      # enough to just check if 'suse_enterprise_storage' is in BarclampCatalog,
      # because that'll be true if the ceph barclamp is installed (as it's
      # a member of suse_enterprise_storage), so we also need to make sure
      # that suse_enterprise_storage is a member of itself, which will only
      # be true if the suse_enterprise_storage barclamp is actually installed.
      BarclampCatalog.barclamps.key?("suse_enterprise_storage") &&
        BarclampCatalog.members("suse_enterprise_storage").key?("suse_enterprise_storage")
    end
  end
end

