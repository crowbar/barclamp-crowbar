#
# Copyright 2011-2013, Dell
# Copyright 2013-2014, SUSE LINUX Products GmbH
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

ActionController::Base.session = {
  :key => "_crowbar_framework_session",
  :secret => "ae66894fcee606d20eab9637a28684a8e9a12d36a91d075035e5703ed49bb26a0f9163fd0954185dea2b701cf79cefad152fb6da0075af43066b790707f52424"
}
ActionController::Base.session_store = :active_record_store
