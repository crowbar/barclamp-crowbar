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
# Author: aabes

module Jobs
  class JobsManager

=begin 
Check if a job can be started. 
A job can be started if:
 - it has no dependencies, or
 - all its dependencies are ready
=end    
    def self.can_job_start(job)
      return true if job.dependencies.nil?      
      ret = true
      job.dependencies.each { |d|
        ret = false if !d.done?        
        break if !ret
      }
      ret
    end

=begin 
 Return a hash of JobID => #of dependencies
=end
    def self.dependency_counts
      Jobs::DependentJob.joins(:dependencies).where(:done=>false).group(Jobs::DependentJob.arel_table[:id]).count
    end
  
=begin 
 Find all the current jobs that can possibly be started.
=end
    def self.find_jobs_to_start
      # This one is a bit wrong, since it performs an inner join (rather than an outer join)
      #Jobs::DependentJob.pending_jobs - Jobs::DependentJob.jobs_with_unready_deps

      # The query below generates an invalid SQL... though it should probably work 
      #Jobs::DependentJob.where(:done=>false).includes( [:dependency_records => :prereq])
      #                  .where("prereqs_dependent_job_dependencies.done='t'",true)
      
      candidates = Jobs::DependentJob.where(:done=>false).includes( [:dependency_records => :prereq])
      to_start = candidates.select { |c| 
        can_job_start(c)
      }
      to_start
    end


    def self.perform_ready_jobs
      jobs = self.find_jobs_to_start
      jobs.each { |j| 
        Rails.logger.info("performing job: #{j.inspect}")
        j.perform()
        j.save!
      }
      jobs
    end    
  end

  
=begin 
A job in the system, that might have un-met dependencies
=end
  class DependentJob < ActiveRecord::Base
    attr_accessible :name, :description
    attr_reader     :done   # is this job complete.
    attr_accessible :type
    attr_accessible :key
    attr_accessible :data

    has_many        :dependency_records, :class_name=> "Jobs::DependentJobDependency", :dependent=>:delete_all
    ## since dependency records are self referential, need to identify the relationship using :source
    has_many        :dependencies, :class_name => Jobs::DependentJob.name, :through=>:dependency_records, :source=>:prereq

    scope :pending_jobs, where(:done => false)    
    scope :jobs_with_ready_deps, pending_jobs.joins(:dependencies).group(:id).having(:done=>true)
    scope :jobs_with_unready_deps, pending_jobs.joins(:dependencies).group(:id).having(:done=>false)

    # set defaults
    before_save :default_values
    def default_values
      if new_record?
        self.done ||= false
        self.type ||= self.class.name
      end
    end

=begin 
Make this job dependent on the one passed in
=end
    def add_dependency(job)
      Jobs::DependentJob.transaction {  
        # d =Jobs::DependentJobDependency.create(:job=>self, :prereq=>job)
        self.dependencies << job
        self.save!
      }
    end

    def set_done(value=true)
      self.done = value
      self.save!
    end

    # No reasonable default action....calling this is probably an error
    def perform()
      raise "Base class has nothing to perform"
    end
  end



=begin 
This is an association class to hold connections between jobs and thier dependencies
=end
  class DependentJobDependency < ActiveRecord::Base
    belongs_to :job, :class_name =>Jobs::DependentJob.name,:inverse_of=>:dependencies
    belongs_to :prereq, :class_name=>Jobs::DependentJob.name

    attr_accessible :job
    attr_accessible :prereq
  end

end

