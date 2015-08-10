class DeployQueueController < ApplicationController
  def index
    @top   = currently_deployed
    @queue = deployment_queue
    @nodes = node_name_map
    @props = prop_name_map
  end

  private

  def node_name_map
    nodes = {}
    NodeObject.all.each { |node| nodes[node.name] = node }
    nodes
  end

  def prop_name_map
    props = {}
    Proposal.all.each { |prop| props["#{prop.barclamp}_#{prop.name}"] = prop }
    props
  end

  def currently_deployed
    Proposal.all.find { |p| p["deployment"][p.barclamp]["crowbar-committing"] }
  end

  def deployment_queue
    ProposalQueue.find("queue").proposals rescue []
  end
end
