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
class JigEvent < ActiveRecord::Base
  attr_accessible :name, :description, :order, :type
  attr_accessible :status, :jig_id

  belongs_to      :snapshot  # the deployment this event will apply

  belongs_to      :jig  # the Jig snapshot this event is being handled by
  has_many        :jig_runs,     :dependent => :destroy
  alias_attribute :runs, :jig_runs

  EVT_UNKNOWN = 0
  EVT_PENDING = 1
  EVT_PROCESSING = 2
  EVT_PARTIAL_COMPLETE =3
  EVT_PARTIAL_FAIL = 4
  EVT_COMPLETE = 5

end
