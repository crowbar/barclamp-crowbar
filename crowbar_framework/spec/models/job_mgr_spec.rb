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
require 'spec_helper'
require 'jobs/job_mgr'



describe "job manager" do

  it "should report jbos without deps as ready" do
    d1 = Jobs::DependentJob.create(:name=>"just do it")
    Jobs::JobsManager.can_job_start(d1).should be true
  end

  it "can record a dependency" do
    d2 = Jobs::DependentJob.create(:name=>"laggered")    
    d1 = Jobs::DependentJob.create(:name=>"just do it")

    d1.add_dependency(d2)
    d1.dependencies.length.should be 1

  end

  it "should report jbos with an unready dependencies as not ready" do
    d2 = Jobs::DependentJob.create(:name=>"laggered")    
    d1 = Jobs::DependentJob.create(:name=>"just do it")
    d1.add_dependency(d2)
    Jobs::JobsManager.can_job_start(d1).should eq(false)

    d2.set_done
    Jobs::JobsManager.can_job_start(d1).should eq(true)
  end

  it "should return correct dependency count" do
    d1 = Jobs::DependentJob.create(:name=>"basic 1")
    d2 = Jobs::DependentJob.create(:name=>"basic 2")
    d3 = Jobs::DependentJob.create(:name=>"basic 3")
    d4 = Jobs::DependentJob.create(:name=>"basic 4")
    d1.add_dependency(d2)
    d3.add_dependency(d4)
    d3.add_dependency(d2)

    counts = Jobs::JobsManager.dependency_counts
    counts[d1.id].should eq 1
    counts[d3.id].should eq 2
  end


  def test_set_membership(set, included,_included=[])
    included.each { |x| set.should      include(x)}
    _included.each { |x| set.should_not include(x) }
  end


  it "should return correct set of tasks to start" do
    d1 = Jobs::DependentJob.create(:name=>"basic 1")
    d2 = Jobs::DependentJob.create(:name=>"basic 2")
    d3 = Jobs::DependentJob.create(:name=>"basic 3")
    d4 = Jobs::DependentJob.create(:name=>"basic 4")
    d1.add_dependency(d2)
    d3.add_dependency(d4)
    d3.add_dependency(d2)

    to_start = Jobs::JobsManager.find_jobs_to_start
    test_set_membership(to_start, [d2,d4], [d1,d3])
    
    d2.set_done
    to_start = Jobs::JobsManager.find_jobs_to_start
    test_set_membership(to_start, [d4], [d1,d2,d3])

  end

  it "should persis subtypes" do
    d1 = Jobs::BaseJob.create(:name=>"base1")
    d1.save!
    d2 = Jobs::TestJob.create(:name=>"test")
    d2.save!

    Jobs::DependentJob.count.should eq(2)
    a = Jobs::DependentJob.all
    test_set_membership(a,[d1,d2])
  end


end

