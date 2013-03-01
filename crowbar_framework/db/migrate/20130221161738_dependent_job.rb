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
class DependentJob < ActiveRecord::Migration
  def change
    create_table :dependent_jobs do |t|
      t.string      :name
      t.string      :description
      t.boolean     :done
      t.timestamps
    end

    create_table :dependent_job_dependencies, :id=>false do |t|
      t.belongs_to  :dependent_job
      t.belongs_to  :prereq

    end
    add_index :dependent_job_dependencies, [:dependent_job_id, :prereq_id], :unique => true, :name =>"jobs_index_unique_ids"
  end
end


