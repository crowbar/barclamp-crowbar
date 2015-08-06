#
# Copyright 2015, SUSE LINUX GmbH
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
  class ProposalQueue

    def initialize
      @db = load_db || create_db
    end

    def proposals
      @db["proposal_queue"]
    end

    def <<(item)
      @db["proposal_queue"] << item
      @db.save
    end

    def save
      @db.save
    end

    def delete(item)
      @db["proposal_queue"].delete_if { |i| i == item }
      @db.save
    end

    def empty?
      @db["proposal_queue"].empty?
    end

    private

    def load_db
      Chef::DataBag.load("crowbar/queue") rescue nil
    end

    def create_db
      db = Chef::DataBagItem.new
      db.data_bag "crowbar"
      db["id"] = "queue"
      db["proposal_queue"] = []
      db.save
    end

  end
end
