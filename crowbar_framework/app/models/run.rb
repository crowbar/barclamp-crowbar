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

  scope :runnable,   -> { where(:running => false).order("id ASC") }
  scope :running,    -> { where(:running => true) }
  scope :running_on, ->(node_id) { running.where(:node_id => node_id) }

  def self.cleanup
    # Clear out any stale runs.
    Run.running.each do |j|
      next if j.node_role.state == NodeRole::TRANSITION rescue Rails.logger.warn("Run job #{j.id} was in queue a nil node_role_id")
      j.destroy
    end
  end

  def self.empty?
    cleanup
    Run.all.count == 0
  end
  
  def self.queued?(nr)
    cleanup
    Run.where(:node_role_id => nr.id).count > 0
  end

  # Queue up a job to run.
  def self.enqueue(nr)
    raise "cannot enqueue a nil node_role!" if nr.nil?
    Rails.logger.info("Run: Starting Run enqueue for #{nr.inspect}")
    if nr.todo? && queued?(nr)
      Rails.logger.info("Run: Already enqueued!")
      return
    end
    Rails.logger.info("Run: Enqueing #{nr.inspect}")
    Run.create!(:node_id => nr.node_id,
                :node_role_id => nr.id)
    run!
  end

  # Run up to maxjobs jobs.
  def self.run!(maxjobs=10)
    cleanup
    queued = 0
    Run.runnable.each do |j|
      next unless Run.running_on(j.node_id).count == 0
      if j.node_role.nil?
        Rails.logger.warn("Run job #{j.id} was in queue a nil node_role_id! removing it")
        j.destroy
      else
        Rails.logger.info("Run: Running #{j.node_role.inspect}")
        j.node_role.state = NodeRole::TRANSITION
        j.running = true
        j.save!
        j.node_role.jig.delay(:queue => "NodeRoleRunner").run(j.node_role)
        queued += 1
        break if queued >= maxjobs
      end
    end
    return queued
  end
end
