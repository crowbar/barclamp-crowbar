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
  class DeploymentQueue
    include CrowbarPacemakerProxy

    attr_reader :logger, :proposal_queue

    def initialize(logger: Rails.logger)
      @logger         = logger
      @proposal_queue = ::ProposalQueue.find("queue")
    end

    # Receives proposal info (name, barclamp), list of nodes (elements), on which the proposal
    # should be applied, and list of dependencies - a list of {barclamp, name/inst} hashes.
    # It adds them to the queue, if possible.
    def queue_proposal(bc, inst, elements, element_order, deps)
      logger.debug("queue proposal: enter #{inst} #{bc}")
      delay = []
      pre_cached_nodes = {}
      begin
        f = file_lock.acquire("queue", logger: logger)
        preexisting_queued_item = @proposal_queue.proposals.detect do |item|
          item["barclamp"] == bc && item["inst"] == inst
        end unless @proposal_queue.empty?

        # If queue_me is true, the delay contains all elements, otherwise, only
        # nodes that are not ready.
        queue_me = !dependencies_satisfied?(deps)

        # Delay is a list of nodes that are not in ready state. pre_cached_nodes
        # is an uninteresting optimization.
        delay, pre_cached_nodes = add_pending_elements(bc, inst, element_order, elements, queue_me)

        # We have all nodes ready.
        if delay.empty?
          # There's a path: process_queue -> proposal_commit -> apply_role ->
          # queue_proposal, which seems to be used as a test if all dependencies
          # (queue_me = false) and nodes (delay.empty?) are still in that state
          # by the time we want to apply. So if that is the case, we just drop
          # proposal from the queue and exit.
          #
          # remove from queue if it was queued before; might not be in the queue
          # because the proposal got changed since it got added to the queue
          unless preexisting_queued_item.nil?
            logger.debug("queue proposal: dequeuing already queued #{inst} #{bc}")
            dequeue_proposal_no_lock(bc, inst)
          end

          return [ delay, pre_cached_nodes ]
        end

        # Delay not empty, we're missing some nodes.
        # And proposal is not in queue
        if preexisting_queued_item.nil?
          @proposal_queue << { "barclamp" => bc, "inst" => inst, "elements" => elements, "deps" => deps }
        else
          # Update (overwrite) item that is already in queue
          preexisting_queued_item["elements"] = elements
          preexisting_queued_item["deps"] = deps
          @proposal_queue.save
        end
      rescue StandardError => e
        logger.error("Error queuing proposal for #{bc}:#{inst}: #{e.message} #{e.backtrace.join("\n")}")
      ensure
        file_lock.release(f)
      end

      # Mark the proposal as in the queue
      prop = Proposal.where(barclamp: bc, name: inst).first
      prop["deployment"][bc]["crowbar-queued"] = true
      prop.save
      logger.debug("queue proposal: exit #{inst} #{bc}")
      [ delay, pre_cached_nodes ]
    end

    # Locking wrapper around dequeue_proposal_no_lock
    def dequeue_proposal(bc, inst)
      logger.debug("dequeue proposal: enter #{inst} #{bc}")
      dequeued = false
      begin
        f = file_lock.acquire("queue", logger: logger)

        if @proposal_queue.empty?
          logger.debug("dequeue proposal: exit #{inst} #{bc}: no entry")
          return [200, {}]
        end

        dequeued = dequeue_proposal_no_lock(bc, inst)
      rescue StandardError => e
        logger.error("Error dequeuing proposal for #{bc}:#{inst}: #{e.message} #{e.backtrace.join("\n")}")
        logger.debug("dequeue proposal: exit #{inst} #{bc}: error")
        return [400, e.message]
      ensure
        file_lock.release(f)
      end
      logger.debug("dequeue proposal: exit #{inst} #{bc}")
      return dequeued ? [200, {}] : [400, '']
    end

    #
    # NOTE: If dependencies don't form a DAG (Directed Acyclic Graph) then we have a problem
    # with our dependency algorithm
    #
    def process_queue
      logger.debug("process queue: enter")
      loop_again = true
      while loop_again
        loop_again = false

        # List contains proposals in the queue that can be applied
        # Remove list contains proposals that were either deleted
        # or should be re-queued?
        # Proposals which reference non-ready nodes are also skipped.
        list = []
        begin
          f = file_lock.acquire("queue", logger: logger)

          if @proposal_queue.empty?
            logger.debug("process queue: exit: empty queue")
            return
          end

          logger.debug("process queue: queue: #{@proposal_queue.proposals.inspect}")

          # Test for ready
          remove_list = []
          @proposal_queue.proposals.each do |item|
            prop = Proposal.where(barclamp: item["barclamp"], name: item["inst"]).first

            if prop.nil?
              remove_list << item
              next
            end

            next unless dependencies_satisfied?(item["deps"])

            nodes_map = elements_to_nodes_to_roles_map(prop["deployment"][item["barclamp"]]["elements"],
                                                       prop["deployment"][item["barclamp"]]["element_order"])
            delay, pre_cached_nodes = elements_not_ready(nodes_map.keys)
            list << item if delay.empty?
          end

          # Update the queue. Drop all proposals that we can process now (list) and those
          # that are deleted (remove_list). This leaves in the queue only proposals
          # which are still waiting for nodes (delay not empty), or for which deps are not
          # ready/created/deployed (queue_me = true).
          remove_list.each do |iii|
            dequeue_proposal_no_lock(iii["barclamp"], iii["inst"])
          end

          list.each do |iii|
            dequeue_proposal_no_lock(iii["barclamp"], iii["inst"])
          end
        rescue StandardError => e
          logger.error("Error processing queue: #{e.message} #{e.backtrace.join("\n")}")
          logger.debug("process queue: exit: error")
          return
        ensure
          file_lock.release(f)
        end

        logger.debug("process queue: list: #{list.inspect}")

        results = commit_proposals(list)

        # 202 means some nodes are not ready, bail out in that case
        # We're re-running the whole apply continuously, until there
        # are no items left in the queue.
        # FIXME: This is lame, because from the user perspective, we're still
        # applying the first barclamp, while this part was in fact already
        # completed and we're applying next item(s) in the queue.
        loop_again = true if results.any? { |state| state != 202 }

        # For each ready item, apply it.
        logger.debug("process queue: exit")
      end
    end

    private

    def commit_proposals(list)
      list.map do |item|
        logger.debug("process queue: item to do: #{item.inspect}")

        bc = item["barclamp"]
        inst = item["inst"]

        service = eval("#{bc.camelize}Service.new logger")

        # This will call apply_role and chef-client.
        # Params: (inst, in_queue, validate_after_save)
        status, message = service.proposal_commit(inst, true, false)

        logger.debug("process queue: item #{item.inspect}: results #{message.inspect}")

        # FIXME: this is perhaps no longer needed
        $htdigest_reload = true

        status
      end
    end

    # Deps are satisfied if all exist, have been deployed and are not in the queue ATM.
    def dependencies_satisfied?(deps)
      deps.all? do |dep|
        depprop = Proposal.where(barclamp: dep["barclamp"], name: dep["inst"]).first
        depprop_queued   = depprop["deployment"][dep["barclamp"]]["crowbar-queued"] rescue false
        depprop_deployed = (depprop["deployment"][dep["barclamp"]]["crowbar-status"] == "success") rescue false

        depprop && !depprop_queued && depprop_deployed
      end
    end


    def file_lock
      FileLock
    end

    # Removes the proposal reference from the queue, updates the proposal as not queued
    # and drops the 'pending roles' from the affected nodes.
    def dequeue_proposal_no_lock(bc, inst)
      logger.debug("dequeue_proposal_no_lock: enter #{inst} #{bc}")
      begin
        # Find the proposal to delete, get its elements (nodes)
        item = @proposal_queue.proposals.detect { |i| i["barclamp"] == bc && i["inst"] == inst }

        if item
          elements = item["elements"]
          @proposal_queue.delete(item)

          # Remove the pending roles for the current proposal from the node records.
          remove_pending_elements(bc, inst, elements) if elements
        end

        # Mark the proposal as not in the queue
        prop = Proposal.where(barclamp: bc, name: inst).first
        unless prop.nil?
          prop["deployment"][bc]["crowbar-queued"] = false
          prop.save
        end
      rescue StandardError => e
        logger.error("Error dequeuing proposal for #{bc}:#{inst}: #{e.message} #{e.backtrace.join("\n")}")
        logger.debug("dequeue proposal_no_lock: exit #{inst} #{bc}: error")
        return false
      end
      logger.debug("dequeue proposal_no_lock: exit #{inst} #{bc}")
      true
    end

    # Each node keeps a list of roles (belonging to the current proposal) that
    # are to be applied to it under crowbar.pending.barclamp-name hash.
    # When we finish deploying and also when we dequeue the proposal, the list
    # should be emptied.  FIXME: looks like bc-inst: value should be a list, not
    # a hash?
    def remove_pending_elements(bc, inst, elements)
      nodes_map = elements_to_nodes_to_roles_map(elements)

      # Remove the entries from the nodes.
      f = file_lock.acquire("BA-LOCK", logger: logger)
      begin
        nodes_map.each do |node_name, data|
          node = NodeObject.find_node_by_name(node_name)
          next if node.nil?
          unless node.crowbar["crowbar"]["pending"].nil? or node.crowbar["crowbar"]["pending"]["#{bc}-#{inst}"].nil?
            node.crowbar["crowbar"]["pending"]["#{bc}-#{inst}"] = {}
            node.save
          end
        end
      ensure
        file_lock.release(f)
      end
    end

    # Create map with nodes and their element list
    # Transform ( {role => [nodes], role1 => [nodes]} hash to { node => [roles], node1 => [roles]},
    # accounting for clusters
    def elements_to_nodes_to_roles_map(elements, element_order = [])
      nodes_map = {}
      active_elements = element_order.flatten

      elements.each do |role_name, nodes|
        next unless active_elements.include?(role_name)

        # Expand clusters to individual nodes
        nodes, failures = expand_nodes_for_all(nodes)
        unless failures.nil? || failures.empty?
          logger.debug "elements_to_nodes_to_roles_map: skipping items that we failed to expand: #{failures.join(", ")}"
        end

        # Add the role to node's list
        nodes.each do |node_name|
          if NodeObject.find_node_by_name(node_name).nil?
            logger.debug "elements_to_nodes_to_roles_map: skipping deleted node #{node_name}"
            next
          end
          nodes_map[node_name] = [] if nodes_map[node_name].nil?
          nodes_map[node_name] << role_name
        end
      end

      nodes_map
    end

    # Get a hash of {node => [roles], node1 => [roles]}
    def add_pending_elements(bc, inst, element_order, elements, queue_me, pre_cached_nodes = {})
      nodes_map = elements_to_nodes_to_roles_map(elements, element_order)

      # We need to be sure that we're the only ones modifying the node records at this point.
      # This will work for preventing changes from rails app, but not necessarily chef.
      # Tough luck.
      f = file_lock.acquire("BA-LOCK", logger: logger)

      # Delay is the list of nodes that are not ready and are needed for this deploy to run
      delay = []
      pre_cached_nodes = {}
      begin
        # Check for delays and build up cache
        # FIXME: why?
        if queue_me
          delay = nodes_map.keys
        else
          delay, pre_cached_nodes = elements_not_ready(nodes_map.keys, pre_cached_nodes)
        end

        unless delay.empty?
          # Update all nodes affected by this proposal deploy (elements) -> add info that this proposal
          # will add list of roles to node's crowbar.pending hash.
          nodes_map.each do |node_name, val|
            # Make sure we have a node.
            node = pre_cached_nodes[node_name]
            node = NodeObject.find_node_by_name(node_name) if node.nil?
            next if node.nil?
            pre_cached_nodes[node_name] = node

            # Mark node as pending. User will be informed about node needing
            # manual allocation if not allocated.
            node.crowbar["crowbar"]["pending"] = {} if node.crowbar["crowbar"]["pending"].nil?
            node.crowbar["crowbar"]["pending"]["#{bc}-#{inst}"] = val
            node.save
          end
        end
      rescue StandardError => e
        logger.fatal("add_pending_elements: Exception #{e.message} #{e.backtrace.join("\n")}")
      ensure
        file_lock.release(f)
      end

      [ delay, pre_cached_nodes ]
    end



    # Assumes the BA-LOCK is held
    def elements_not_ready(nodes, pre_cached_nodes = {})
      # Check to see if we should delay our commit until nodes are ready.
      delay = []
      nodes.each do |n|
        node = NodeObject.find_node_by_name(n)
        next if node.nil?

        pre_cached_nodes[n] = node
        delay << n if node.crowbar['state'] != "ready" and !delay.include?(n)
      end
      [ delay, pre_cached_nodes ]
    end

  end
end
