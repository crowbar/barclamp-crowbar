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
  # Stores status and message on the delivered proposal_config
  #
  def self.update_proposal_status(pc, status, message)
    res = true
    if pc
      pc.status = status
      pc.failed_reason = message
      res = pc.save
    end
    res
  end

  #
  # Helper function to set logging structure
  #
  def logger
    @logger
  end
  def logger=(log)
    @logger = log
  end

  #
  # Helper routine to test if nodes are ready or not.
  # Returns a list of nodes not ready.
  #
  # This is soft and not locked.  The apply path will lock and confirm.
  #
  def self.elements_not_ready(nodes)
    return [] unless nodes
    delay = []
    nodes.each do |n|
      next if n.state == "ready"
      delay << "Node #{n.name}"
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
  #   in_nodes - list of Node objects to test
  #   apply - if all are ready, mark it applying
  #
  # Output:
  #   List of nodes that are NOT ready [ "Node <node.name>" ]
  #   If an exception occurs, delay will contain "Error: Message" in the list.
  #
  def self.make_applying_or_delay(in_nodes, apply)
    return [] unless in_nodes
    nodes = in_nodes.uniq

    delay = []
    begin
      # Check for delays
      delay = ProposalQueue.elements_not_ready(nodes)

      # Add the entries to the nodes.
      if delay.empty?
	if apply
	  list = []
	  nodes.each do |node|
	    # Nothing to delay so mark them applying.
	    answer, message = node.set_state('applying', 'ready')
	    list << node if answer < 300
	    delay << "Node #{node.name}" if answer == 409
	  end

	  # Error on setting state
	  if list.length != nodes.length
	    list.each do |node|
	      node.set_state('ready', 'applying')
	    end
	  end
	end
      end

      unless delay.empty?
	nodes.each do |node|
	  # Make sure the node is allocated
	  node.allocate
	end
      end
    rescue Exception => e
      delay << "Error: #{e.message} #{e.backtrace.join("\n")}"
    end

    delay
  end

  #
  # Helper routine to put the nodes back to ready
  #
  # Input: list of node objects
  # Output: none
  #
  def self.restore_to_ready(nodes)
    nodes.each do |node|
      # Nothing to delay so mark them applying.
      node.set_state('ready','applying')
    end
  end

  #
  # Helper function:
  #
  # Test if proposal_config can run
  # return: next insertion point, delay list
  #
  def can_proposal_config_run(prop_config, should_apply)
    # Make sure the deps if we aren't being queued.
    delay = []
    pos = 1
    queue_me = false
    deps = prop_config.proposal.barclamp.operations(@logger).proposal_dependencies(prop_config)
    deps.each do |dep|
      bc_name = dep["barclamp"]
      inst_name = dep["inst"]
      prop = Proposal.find_by_name_and_barclamp_id(inst_name, Barclamp.find_by_name(bc_name).id)

      # Make sure we an applied active proposal
      unless prop and prop.active? and prop.active_config.applied?
	queue_me = true
	delay << "Proposal #{bc_name}.#{inst_name}"
	if prop
	  pac_id = prop.active? ? prop.active_config.id : prop.current_config.id
	  item = ProposalQueueItem.find_by_proposal_config_id_and_proposal_queue_id(pac_id, self.id)
	  npos = item ? item.position + 1 : 0
	  pos = npos if pos < npos
	end
      end
    end

    # Check nodes for being ready
    answer = false
    answer = !queue_me if should_apply
    delay << ProposalQueue.make_applying_or_delay(prop_config.nodes, answer)
    [ pos, delay.flatten ]
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
  # Returns:
  #   [ true/false, String ]
  #   true = proposal was queued for string reason
  #   false = proposal was NOT queued.
  #
  def queue_proposal(prop_config)
    inst = prop_config.proposal.name
    bc = prop_config.proposal.barclamp.name
    delay = []

    @logger.debug("queue proposal: enter #{inst} #{bc}")
    CrowbarUtils.with_lock("queue") do
      begin
	item = ProposalQueueItem.find_by_proposal_config_id_and_proposal_queue_id(prop_config.id, self.id)
	if item
	  @logger.debug("queue proposal: exit #{inst} #{bc}: already queued")
	  return [true, item.queue_reason]
	end

	pos, delay = can_proposal_config_run(prop_config, true)

	# Nothing stopping use and nodes are marked applying.
	return [ false, "" ] if delay.empty?

	pqi = ProposalQueueItem.new
	pqi.proposal_config_id = prop_config.id
	pqi.queue_reason = delay.join(",")
	pqi.position = pos
	pqi.proposal_queue_id = self.id
	pqi.save!

	ProposalQueue.update_proposal_status(prop_config, ProposalConfig::STATUS_QUEUED, "")
      rescue Exception => e
	@logger.error("Error queuing proposal for #{bc}:#{inst}: #{e.message}")
	delay << "Error: #{bc}:#{inst}: #{e.message}"
      end
    end

    @logger.debug("queue proposal: exit #{inst} #{bc}")
    [ true, delay.join(",") ]
  end

  #
  # Take a proposal off the queue (assumes lock is already held) and update the proposal's status
  #
  def dequeue_proposal_no_lock(item)
    @logger.debug("dequeue_proposal_no_lock: enter #{item}")
    begin
      if item
	ProposalQueue.update_proposal_status(item.proposal_config, ProposalConfig::STATUS_NONE, "")
	item.destroy
      end
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
    @logger.debug("dequeue proposal: enter #{bc}:#{inst}")
    ret = false
    CrowbarUtils.with_lock("queue") do
      begin
	prop = Proposal.find_by_name_and_barclamp_id(inst, Barclamp.find_by_name(bc).id)
	return true unless prop
	return true unless prop.active?

	item = ProposalQueueItem.find_by_proposal_config_id(prop.active_config.id)
	ret = dequeue_proposal_no_lock(item)
      rescue Exception => e
	@logger.error("Error dequeuing proposal for #{bc}:#{inst}: #{e.message} #{e.backtrace}")
	@logger.debug("dequeue proposal: exit #{inst} #{bc}: error")
	return ret
      end
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
  # returns: [ true/false, count, message ]
  #   true for success and false for failure
  #   count is number of items processed
  #   message is an error message.
  #
  def process_queue
    @logger.debug("process queue: enter")
    loop_again = true
    master_count = 0
    while loop_again
      loop_again = false
      list = []
      CrowbarUtils.with_lock("queue") do
	begin
	  items = proposal_queue_items
	  if items.nil? or items.empty?
	    @logger.debug("process queue: exit: empty queue")
	    return [ true, master_count, "" ]
	  end

	  @logger.debug("process queue: queue: #{items.inspect}")

	  # Test for ready
	  remove_list = []
	  items.each do |item|
	    prop_config = item.proposal_config
	    pos, delay = can_proposal_config_run(prop_config, false)
	    next unless delay.empty?
	    list << item
	  end

	  list.each do |iii|
	    master_count += 1
	    dequeue_proposal_no_lock(iii)
	  end

	rescue Exception => e
	@logger.error("Error processing queue: #{e.message}")
	@logger.debug("process queue: exit: error")
	return [ false, master_count, "Error: #{e.message}" ]
	end
      end

      @logger.debug("process queue: list: #{list.inspect}")

      # For each ready item, apply it.
      list.each do |item|
	@logger.debug("process queue: item to do: #{item.inspect}")
	p = item.proposal_config.proposal
	answer = p.barclamp.operations(@logger).proposal_commit(p.name, true)
	@logger.debug("process queue: item #{item.inspect}: results #{answer.inspect}")
	loop_again = true if answer[0] != 202
      end
      reload if loop_again # Make sure we get latest objects
    end
    @logger.debug("process queue: exit")
    [ true, master_count, "" ]
  end

end
