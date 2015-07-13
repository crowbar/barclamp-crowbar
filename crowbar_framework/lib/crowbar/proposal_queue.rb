module Crowbar
  class ProposalQueue

    def initialize
      @db = load_db || create_db
    end

    def proposals
      @db["proposal_queue"]
    end

    def <<(item)
      @db["proposal_queue"] << item
      @db.save
    end

    def save
      @db.save
    end

    def delete(item)
      @db["proposal_queue"].delete_if { |i| i == item }
      @db.save
    end

    def empty?
      @db["proposal_queue"].empty?
    end

    private

    def load_db
      Chef::DataBag.load("crowbar/queue") rescue nil
    end

    def create_db
      db = Chef::DataBagItem.new
      db.data_bag "crowbar"
      db["id"] = "queue"
      db["proposal_queue"] = []
      db.save
    end

  end
end
