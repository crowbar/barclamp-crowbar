class ProposalQueue < ActiveRecord::Base
  # FIXME: remove this when the export is properly implemented
  class_attribute :chef_type

  self.chef_type = "data_bag_item"
  # only accept one default record named "queue"
  self.primary_key = "name"

  serialize :properties, Utils::JSONWithIndifferentAccess

  def proposals
    proposal_queue.properties["proposal_queue"]
  end

  def <<(item)
    pq = proposal_queue
    pq.properties["proposal_queue"] << item
    pq.save
  end

  def save
    self.save!
  end

  def delete(item)
    pq = proposal_queue
    pq.properties["proposal_queue"].delete_if { |i| i == item }
    pq.save
  end

  def empty?
    proposal_queue.properties["proposal_queue"].empty?
  end

  private

  def proposal_queue
    ProposalQueue.find("queue")
  end
end
