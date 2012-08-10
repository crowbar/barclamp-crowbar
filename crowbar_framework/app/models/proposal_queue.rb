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

class ProposalQueue < ActiveRecord::Base
  attr_accessible :name

  has_many :proposal_queue_items, :order => "position"

  def self.get_queue(name, logger)
    q = ProposalQueue.find_or_create_by_name(name)
    q.logger = logger
    q
  end

  #
  # update proposal status information
  #
  def self.update_proposal_status(inst, status, message, bc)
    bc_id = Barclamp.find_by_name(bc)
    prop = Proposal.find_by_name_and_barclamp_id(inst, bc_id)
    res = true
    if prop and prop.active?
      prop.active_config.status = status
      prop.active_config.failed_reason = message
      res = prop.active_config.save!
    end
    res
  end

  def logger=(log)
    @logger = log
  end

  #
  # Locking Routines
  #
  def acquire_lock(name)
    @logger.debug("Acquire #{name} lock enter")
    f = File.new("tmp/#{name}.lock", File::RDWR|File::CREAT, 0644)
    @logger.debug("Acquiring #{name} lock")
    rc = false
    count = 0
    while rc == false do
      count = count + 1
      @logger.debug("Attempt #{name} Lock: #{count}")
      rc = f.flock(File::LOCK_EX|File::LOCK_NB)
      sleep 1 if rc == false
    end
    @logger.debug("Acquire #{name} lock exit: #{f.inspect}, #{rc}")
    f
  end

  def release_lock(f)
    @logger.debug("Release lock enter: #{f.inspect}")
    f.flock(File::LOCK_UN)
    f.close
    @logger.debug("Release lock exit")
  end

  # Assumes the BA-LOCK is held
  def elements_not_ready(nodes)
    delay = []
    nodes.each do |n|
      next if node.state == "ready"
      delay << "Node node.name"
    end
    delay
  end

  def make_applying_or_delay(nodes, queue_me, set_apply)
    f = acquire_lock "BA-LOCK"
    delay = []
    begin
      # Check for delays 
      delay = elements_not_ready(nodes)

      # Add the entries to the nodes.
      if delay.empty?
        if set_apply
          nodes.each do |node|
            # Nothing to delay so mark them applying.
            node.state = 'applying'
            node.save
          end
        end
      else
        nodes.each do |node|
          # Make sure the node is allocated
          node.allocated = true
          node.save
        end
      end
    rescue Exception => e
      @logger.fatal("add_pending_elements: Exception #{e.message} #{e.backtrace}")
    ensure
      release_lock f
    end

    delay
  end

  def restore_to_ready(nodes)
    f = acquire_lock "BA-LOCK"
    begin
      nodes.each do |node|
        # Nothing to delay so mark them applying.
        node.state = 'ready'
        node.save
      end
    ensure
      release_lock f
    end
  end

  #
  # Queuing routines:
  #   queue_proposal - attempts to queue proposal returns delay otherwise.
  #   dequeue_proposal - remove item from queue and clean up
  #   process_queue - see what we can execute
  #
  def queue_proposal(prop_config)
    inst = prop_config.proposal.name
    bc = prop_config.proposal.barclamp.name
    @logger.debug("queue proposal: enter #{inst} #{bc}")
    begin
      f = acquire_lock "queue"

      proposal_queue_items.each do |item|
        if prop_config.id == item.proposal_config_id
          @logger.debug("queue proposal: exit #{prop_config.name} #{prop_config.barclamp.name}: already queued")
          return [true, item.queue_reason]
        end
      end

      # Make sure the deps if we aren't being queued.
      delay = []
      deps = prop_config.prop.barclamp.operations(@logger).proposal_dependencies(prop_config)
      deps.each do |dep|
        prop = Proposal.find_by_name_and_barclamp_id(dep["inst"], Barclamp.find_by_name(dep["barclamp"].id))

        # Make sure we an applied active proposal
        unless prop and prop.active? and prop.active_config.applied?
          queue_me = true
          delay << "Proposal #{prop.barclamp.name}.#{prop.name}"
        end
      end

      # Check nodes for being ready
      delay << make_applying_or_delay(prop_config.nodes, queue_me, true)
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
      release_lock f
    end

    update_proposal_status(prop_config.prop.name, ProposalConfig::STATUS_QUEUED, "", prop_config.prop.barclamp.name)
    @logger.debug("queue proposal: exit #{inst} #{bc}")
    delay
  end

  def dequeue_proposal_no_lock(item)
    @logger.debug("dequeue_proposal_no_lock: enter #{item}")
    begin
      update_proposal_status(item.proposal_config.prop.name, ProposalConfig::STATUS_NONE, "", 
                             item.proposal_config.prop.barclamp.name)
      item.destroy
    rescue Exception => e
      @logger.error("Error dequeuing proposal for #{item}: #{e.message} #{e.backtrace}")
      @logger.debug("dequeue proposal_no_lock: exit #{item}: error")
      return false
    end
    @logger.debug("dequeue proposal_no_lock: exit #{item}")
    true
  end

  def dequeue_proposal(inst, bc = @bc_name)
    @logger.debug("dequeue proposal: enter #{item}")
    ret = false
    begin
      f = acquire_lock "queue"

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
      release_lock f
    end
    @logger.debug("dequeue proposal: exit #{inst} #{bc}")
    ret
  end

  #
  # NOTE: If dependencies don't form a DAG (Directed Acyclic Graph) then we have a problem
  # with our dependency algorithm
  #
  def process_queue
    @logger.debug("process queue: enter")
    loop_again = true
    while loop_again
      loop_again = false
      list = []
      begin
        f = acquire_lock "queue"

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
          deps = prop_config.prop.barclamp.operations(@logger).proposal_dependencies(prop_config)
          deps.each do |dep|
            prop = Proposal.find_by_name_and_barclamp_id(dep["inst"], Barclamp.find_by_name(dep["barclamp"].id))

            # Make sure we an applied active proposal
            unless prop and prop.active? and prop.active_config.applied?
              queue_me = true
              delay << "Proposal #{prop.barclamp.name}.#{prop.name}"
            end
          end

          # Check nodes for being ready
          delay << make_applying_or_delay(prop_config.nodes, queue_me, false)
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
        release_lock f
      end

      @logger.debug("process queue: list: #{list.inspect}")

      # For each ready item, apply it.
      list.each do |item|
        @logger.debug("process queue: item to do: #{item.inspect}")
        answer = item.prop_config.prop.barclamp.operations(@logger).proposal_commit(prop_config.prop.name, true)
        @logger.debug("process queue: item #{item.inspect}: results #{answer.inspect}")
        loop_again = true if answer[0] != 202
      end
      @logger.debug("process queue: exit")
    end
  end

end

