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

  belongs_to :jig  # the Jig instance this event is being handled by
  belongs_to :proposal_config  # the configuration this event will apply
  has_many   :jig_run         # the runs that this event needs to execute

  # 
  # Validates that Event Names are useful placeholders.  They are 
  # mainly referenced by their ID. 
  # and that it starts with an alph and only contains alpha,digist,hyphen,underscore
  #
  validates_uniqueness_of :name, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  validates_format_of :name, :with=> /^[a-zA-Z][_a-zA-Z0-9]*$/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")
  
  #TEMPORARY REMOVAL... belongs_to :jig_map 
  #has_many :jig_events

  EVT_PENDING = 1
  EVT_PROCESSING = 2
  EVT_PARTIAL_COMPLETE =3
  EVT_PARTIAL_FAIL = 4
  EVT_COMPLETE = 5


  def init
    puts "JigEvent init."
  end

  def run
    puts "creating a JigRun"

    self.save!
    e = JigRun.new
    r.jig_run_id = self.id
    r
  end

  def as_json options={}
    {
      :name=> name,
      :description=> description,
      :order=> order,
      :type=> type,
      :id=> id,
      :created_at=> created_at,
      :updated_at=> updated_at
    }
  end
end
