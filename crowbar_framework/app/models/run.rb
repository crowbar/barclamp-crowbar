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

class Run < ActiveRecord::Base

  belongs_to :node
  belongs_to :node_role

  attr_accessible :node_role_id, :node_id

  scope :runnable,   -> { where(:running => false).sort{|a,b| a.sort_id <=> b.sort_id} }
  scope :running,    -> { where(:running => true) }
  scope :running_on, ->(node_id) { running.where(:node_id => node_id) }
  scope :deletable,  -> { find_by_sql(%Q{select runs.* from runs INNER JOIN node_roles
          ON runs.node_role_id = node_roles.id
          where NOT ((node_roles.state = #{NodeRole::TRANSITION}) OR
         (node_roles.state in (#{NodeRole::TODO}, #{NodeRole::ACTIVE}) AND NOT runs.running))}) }

  def sort_id
    [node_role.cohort, node_role_id, id]
  end

  def self.cleanup
    # Clear out any stale runs.
    # The only thing allowed on the run qudefaulteue are:
    # Runs that are in state TRANSITION
    # Runs that are not running with a noderole in TODO or ACTIVE
    Run.transaction do
      ActiveRecord::Base.connection.execute("LOCK TABLE runs")
      deletable.each do |j|
        Rails.logger.info("Run: Deleting #{j.node_role.name}: state #{j.node_role.state}") unless j.node_role.nil?
        j.destroy
      end
    end
  end

  def self.empty?
    cleanup
    Run.transaction do
      ActiveRecord::Base.connection.execute("LOCK TABLE runs")
      Run.all.count == 0
    end
  end


  # Queue up a job to run.
  # Run.enqueue should only be called when you want to enqueue a noderole
  # that is in a state other than TODO, as those will be picked up by Run.run!
  # The main callers of this should mostly be events called from role triggers.
  def self.enqueue(nr)
    raise "cannot enqueue a nil node_role!" if nr.nil?
    cleanup
    Run.transaction do
      unless nr.runnable? &&
          ([NodeRole::ACTIVE, NodeRole::TODO, NodeRole::TRANSITION].member?(nr.state))
        Rails.logger.info("Run: #{nr.name} is NOT enqueueable/runnable [nr.state #{nr.state} is Active/Todo/Trans && node.available #{nr.node.available} && node.alive #{nr.node.alive} && jig.active #{nr.role.jig.active}]")
      else
        ActiveRecord::Base.connection.execute("LOCK TABLE runs")
        current_run = Run.where(:node_id => nr.node_id).first
        if nr.todo? && !current_run.nil?
          Rails.logger.info("Run: #{nr.name} in TODO and #{current_run.node_role.name} is already enqueued on #{nr.node.name}")
        else
          if current_run
            Rails.logger.info("Run: Enqueing #{nr.name} after #{current_run.node_role.name}")
          else
            Rails.logger.info("Run: Enqueing #{nr.name}")
          end
          Run.create!(:node_id => nr.node_id,
                      :node_role_id => nr.id)
        end
      end
      Rails.logger.info("Run: Queue: #{Run.all.map{|j|"#{j.node_role.name}: state #{j.node_role.state}"}}") rescue Rails.logger.info("Run: enqueue has nil node_roles")
    end
    run!
  end

  # Run up to maxjobs jobs, enqueuing runnable noderoles in TODO as it goes.
  def self.run!(maxjobs=10)
    queued = 0
    cleanup
    Run.transaction do
      # We need an exclusive lock to the table to run anything.
      ActiveRecord::Base.connection.execute("LOCK TABLE runs")
      # Find any runnable noderoles and see if they can be enqueued.
      # The logic here will only enqueue a noderole of the node does not
      # already have a noderole enqueued.
      NodeRole.runnable.order("cohort ASC, id ASC").each do |nr|
        if Run.where(:node_id => nr.node_id).count > 0
          Rails.logger.info("Run: Skipping #{nr.name}")
        else
          Rails.logger.info("Run: Enqueing #{nr.name}")
          Run.create!(:node_id => nr.node_id,
                      :node_role_id => nr.id)
        end
      end
      runnable = Run.runnable.count
      return if runnable == 0
      # Now that we have things that are runnable, loop through them to see
      # what we can actually run.
      Run.runnable.each do |j|
        # If one of our candidates refers to a node that already has something
        # running on it, then skip it for now.
        next unless Run.running_on(j.node_id).count == 0
        raise "you cannot run job #{j.id} on node #{j.node_id} without a node_role." if j.node_role.nil?
        Rails.logger.info("Run: Sending #{j.node_role.name} to delayed_jobs")
        j.node_role.state = NodeRole::TRANSITION
        j.running = true
        j.save!
        # Destructive noderoles only run once.  Enforce that here.
        if j.node_role.role.destructive && j.node_role.run_count > 0
          Rails.logger.info("Run: #{j.node_role.name} is destructive and has already run.")
          j.node_role.state = NodeRole::ACTIVE
          j.node_role.save!
          next
        end
        # Take a snapshot of the data we want to hand to the jig's run method.
        # We do this so that the jig gets fed data that is consistent for this point
        # in time, as opposed to picking up whatever is lying around when delayed_jobs
        # gets around to actually doing its thing, which may not be what we expect.
        begin
          run_data = j.node_role.jig.stage_run(j.node_role)
          j.node_role.jig.delay(:queue => "NodeRoleRunner").run(j.node_role,run_data)
          queued += 1
        rescue Exception => e
          j.node_role.runlog = "#{e.message}\n#{e.backtrace.join("\n")}"
          j.node_role.state = NodeRole::ERROR
          j.node_role.save!
        end
        break if queued >= maxjobs
      end if runnable > 0
      Rails.logger.info("Run: #{runnable} runnable, #{queued} handled this pass, #{Run.running.count} in delayed_jobs")
      begin
        # log queue state
        Rails.logger.debug("Run: Queue: #{Run.all.map{|j|"#{j.node_role.name}: state #{j.node_role.state}"}}") 
      rescue
        # catch node_role is nil (exposed in simulator runs)
        Run.all.each { |j| raise "you cannot run job #{j.id} with missing node #{j.node_id} and node_role #{j.node_role_id} information.  This is likely a garbage collection issue!" if j.node_role.nil? }
      end
      return queued
    end
  end
end
