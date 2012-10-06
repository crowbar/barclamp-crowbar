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

#
# This class acts as a queue with bonus routines.
#
# A proposal queue is a place to queue proposals that don't have
# their dependencies met.  This is not really generic.  It is very
# specific to proposal and its queuing.
#
# You can create a queue by calling: ProposalQueue.get_queue(name, logger)
#
# A queue has the following actions:
#   queue_proposal - attempts to queue proposal returns delay otherwise.
#   dequeue_proposal - remove item from queue and clean up
#   process_queue - see what we can execute
#
# The other public routine is:
#   ProposalQueue.update_proposal_statue - this updates the status of a proposal with the mssage.
#
# The other routines are internal and have docs around them.
#
class ProposalQueue < ActiveRecord::Base
  attr_accessible :name

  #
  # Single queue of proposals.  The proposal_queue_item model
  # is a helper structure to maintain an ordered list.
  #
  has_many :proposal_queue_items, :order => "position"

  #
  # Get get_queue will return the queue by name (or creates it).
  #
  def self.get_queue(name, logger)
    q = ProposalQueue.find_or_create_by_name(name)
    q.logger = logger
    q
  end

  #
  # update proposal status information
  # Stores status and message on the current active_config
  #
  def self.update_proposal_status(pc, status, message)
    prop = pc.proposal
    res = true
    if prop and prop.active?
      prop.active_config.status = status
      prop.active_config.failed_reason = message
      res = prop.active_config.save!
    end
    res
  end

  #
  # Helper function to set logging structure
  #
  def logger=(log)
    @logger = log
  end

  #
  # Helper function to test if a list of nodes are ready.
  #
  # Input: List of node objects
  # Output: List of nodes that are NOT ready [ "Node <node.name>" ]
  #
  # Assumes the BA-LOCK is held
  #
  def elements_not_ready(nodes)
    delay = []
    nodes.each do |n|
      next if node.state == "ready"
      delay << "Node node.name"
    end
    delay
  end

  #
  # Helper routine to test if nodes are available and if they are potentially
  # mark them as applying.
  #
  # If apply is true, mark nodes otherwise just build delay list.
  #
  # Input:
  #   nodes - list of Node objects to test
  #   apply - if all are ready, mark it applying
  #
  # Output:
  #   List of nodes that are NOT ready [ "Node <node.name>" ]
  #
  def make_applying_or_delay(nodes, apply)
    f = CrowbarUtils.acquire_lock "BA-LOCK"
    delay = []
    begin
      # Check for delays 
      delay = elements_not_ready(nodes)

      # Add the entries to the nodes.
      if delay.empty?
        if apply
          nodes.each do |node|
            # Nothing to delay so mark them applying.
            node.set_state('applying')
          end
        end
      else
        nodes.each do |node|
          # Make sure the node is allocated
          node.allocate
        end
      end
    rescue Exception => e
      @logger.fatal("add_pending_elements: Exception #{e.message} #{e.backtrace}")
    ensure
      CrowbarUtils.release_lock f
    end

    delay
  end

  #
  # Helper routine to put the nodes back to ready
  #
  # Input: list of node objects
  # Output: none
  #
  def restore_to_ready(nodes)
    f = CrowbarUtils.acquire_lock "BA-LOCK"
    begin
      nodes.each do |node|
        # Nothing to delay so mark them applying.
        node.set_state('ready')
      end
    ensure
      CrowbarUtils.release_lock f
    end
  end

  #
  # Public queuing routines:
  #   queue_proposal - attempts to queue proposal returns delay otherwise.
  #   dequeue_proposal - remove item from queue and clean up
  #   process_queue - see what we can execute
  #
  # queue_proposal 
  #
  # Processes the proposal_config and validates is dependencies and node readiness.
  # If the node is not ready, it is queued until nodes or proposal become ready.
  #
  #
  def queue_proposal(prop_config)
    inst = prop_config.proposal.name
    bc = prop_config.proposal.barclamp.name
    @logger.debug("queue proposal: enter #{inst} #{bc}")
    begin
      f = CrowbarUtils.acquire_lock "queue"

      proposal_queue_items.each do |item|
        if prop_config.id == item.proposal_config_id
          @logger.debug("queue proposal: exit #{prop_config.name} #{prop_config.barclamp.name}: already queued")
          return [true, item.queue_reason]
        end
      end

      # Make sure the deps if we aren't being queued.
      delay = []
      queue_me = false
      deps = prop_config.proposal.barclamp.operations(@logger).proposal_dependencies(prop_config)
      deps.each do |dep|
        prop = Proposal.find_by_name_and_barclamp_id(dep["inst"], Barclamp.find_by_name(dep["barclamp"].id))

        # Make sure we an applied active proposal
        unless prop and prop.active? and prop.active_config.applied?
          queue_me = true
          delay << "Proposal #{prop.barclamp.name}.#{prop.name}"
        end
      end

      # Check nodes for being ready
      delay << make_applying_or_delay(prop_config.nodes, !queue_me)
      delay = delay.flatten

      # Nothing stopping use and nodes are marked applying.
      return [ false, "" ] if delay.empty?

      pqi = ProposalQueueItem.create(:proposal_config_id => prop_config.id)
      pqi.queue_reason = delay.join(",")
      pqi.position = 12 # GREG: Fix this.
      proposal_queue_items << pqi
      save!
    rescue Exception => e
      @logger.error("Error queuing proposal for #{bc}:#{inst}: #{e.message}")
    ensure
      CrowbarUtils.release_lock f
    end

    self.update_proposal_status(prop_config, ProposalConfig::STATUS_QUEUED, "")
    @logger.debug("queue proposal: exit #{inst} #{bc}")
    delay
  end

  #
  # Take a proposal off the queue (assumes lock is already held) and update the proposal's status
  #
  def dequeue_proposal_no_lock(item)
    @logger.debug("dequeue_proposal_no_lock: enter #{item}")
    begin
      self.update_proposal_status(item.proposal_config, ProposalConfig::STATUS_NONE, "")
      item.destroy
    rescue Exception => e
      @logger.error("Error dequeuing proposal for #{item}: #{e.message} #{e.backtrace}")
      @logger.debug("dequeue proposal_no_lock: exit #{item}: error")
      return false
    end
    @logger.debug("dequeue proposal_no_lock: exit #{item}")
    true
  end

  #
  # Remove a proposal_config from the queue.  This will take the BA_LOCK
  #
  # Input:
  #   inst - name of proposal
  #   bc = name of barclamp 
  # Assumes to dequeue the active_config on the proposal.
  #
  def dequeue_proposal(inst, bc)
    @logger.debug("dequeue proposal: enter #{item}")
    ret = false
    begin
      f = CrowbarUtils.acquire_lock "queue"

      prop = Proposal.find_by_name_and_barclamp_id(inst, Barclamp.find_by_name(bc).id)
      return true unless prop
      return true unless prop.active?
      
      item = ProposalQueueItem.find_by_proposal_config_id(prop.active_config.id)
      ret = dequeue_proposal_no_lock(item)
    rescue Exception => e
      @logger.error("Error dequeuing proposal for #{bc}:#{inst}: #{e.message} #{e.backtrace}")
      @logger.debug("dequeue proposal: exit #{inst} #{bc}: error")
      return ret
    ensure
      CrowbarUtils.release_lock f
    end
    @logger.debug("dequeue proposal: exit #{inst} #{bc}")
    ret
  end

  #
  # NOTE: If dependencies don't form a DAG (Directed Acyclic Graph) then we have a problem
  # with our dependency algorithm
  #
  # process_queue - public routine to check the queue if other proposal_configs are able to
  # be run.  If they are ready, they are dequeued and proposal_commit is called on them.
  #
  def process_queue
    @logger.debug("process queue: enter")
    loop_again = true
    while loop_again
      loop_again = false
      list = []
      begin
        f = CrowbarUtils.acquire_lock "queue"

        if proposal_queue_items.nil? or proposal_queue_items.empty?
          @logger.debug("process queue: exit: empty queue")
          return
        end

        @logger.debug("process queue: queue: #{proposal_queue_items.inspect}")

        # Test for ready
        remove_list = []
        proposal_queue_items.each do |item|
          queue_me = false
          prop_config = item.proposal_config

          # Make sure the deps if we aren't being queued.
          delay = []
          deps = prop_config.proposal.barclamp.operations(@logger).proposal_dependencies(prop_config)
          deps.each do |dep|
            prop = Proposal.find_by_name_and_barclamp_id(dep["inst"], Barclamp.find_by_name(dep["barclamp"].id))

            # Make sure we an applied active proposal
            unless prop and prop.active? and prop.active_config.applied?
              queue_me = true
              delay << "Proposal #{prop.barclamp.name}.#{prop.name}"
            end
          end

          # Check nodes for being ready
          delay << make_applying_or_delay(prop_config.nodes, false)
          delay = delay.flatten

          # We are free
          next unless delay.empty?
          list << item 
        end

        list.each do |iii| 
          dequeue_proposal_no_lock(iii)
        end
      
      rescue Exception => e
        @logger.error("Error processing queue: #{e.message}")
        @logger.debug("process queue: exit: error")
        return
      ensure
        CrowbarUtils.release_lock f
      end

      @logger.debug("process queue: list: #{list.inspect}")

      # For each ready item, apply it.
      list.each do |item|
        @logger.debug("process queue: item to do: #{item.inspect}")
        answer = item.prop_config.proposal.barclamp.operations(@logger).proposal_commit(prop_config.proposal.name, true)
        @logger.debug("process queue: item #{item.inspect}: results #{answer.inspect}")
        loop_again = true if answer[0] != 202
      end
      @logger.debug("process queue: exit")
    end
  end

end

