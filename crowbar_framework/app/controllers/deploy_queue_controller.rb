class DeployQueueController < ApplicationController
  def index
    @top   = ProposalObject.all.find { |p| p["deployment"][p.barclamp]["crowbar-committing"] }
    @queue = ProposalObject.find_data_bag_item("crowbar/queue")["proposal_queue"] rescue []
    @nodes = {}
    @props = {}
    NodeObject.all.each { |node| @nodes[node.name] = node }
    ProposalObject.all.each { |prop| @props["#{prop.barclamp}_#{prop.name}"] = prop }
  end
end
