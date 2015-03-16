module Crowbar
  module ProposalMethods
    def raw_attributes
      @raw_attributes ||= begin
        raw_data["attributes"][self.barclamp] || {}
      end
    end

    def pretty_attributes
      @pretty_attributes ||= begin
        Utils::ExtendedHash.new(
          raw_attributes.dup
        )
      end
    end

    def pretty_attributes_json
      JSON.pretty_generate(
        JSON.parse(
          (raw_data["attributes"][barclamp] || {}).to_json
        )
      )
    end

    def raw_deployment
      @raw_deployment ||= begin
        raw_data["deployment"][barclamp] || {}
      end
    end

    def pretty_deployment
      @pretty_deployment ||= begin
        Utils::ExtendedHash.new(
          raw_data["deployment"][barclamp].dup
        )
      end
    end

    def pretty_deployment_json
      JSON.pretty_generate(
        JSON.parse(
          (raw_data["deployment"][barclamp] || {}).to_json
        )
      )
    end

    def category
      @category ||= BarclampCatalog.category(barclamp)
    end

    def item
      @item
    end

    def prop
      [barclamp, name].join("_")
    end

    def display_name
      @display_name ||= BarclampCatalog.display_name(barclamp)
    end

    def allow_multiple_proposals?
      ServiceObject.get_service(barclamp).allow_multiple_proposals?
    end

    #NOTE: Status is NOT accurate if the proposal has been deactivated!  You must check the role.
    def status
      bc = item["deployment"][self.barclamp]
      if bc.nil?
        "hold"
      else
        return "unready" if bc["crowbar-committing"]
        return "pending" if bc["crowbar-queued"]
        return "hold" if !bc.has_key? "crowbar-queued" and !bc.has_key? "crowbar-committing"
        if !bc.key? "crowbar-status" or bc["crowbar-status"] === "success"
          "ready"
        else
          "failed"
        end
      end
    end
    
    # nil if not appliciable, true = if success, false if failed
    def failed?
       status === 'failed'
    end

    # for locationlization, will lookup text before the :  
    def fail_reason
       s = if failed?
         item["deployment"][self.barclamp]["crowbar-failed"].to_s
       elsif status === "ready"
         "Did not fail.  Successfully applied: #{barclamp}-#{name} (status #{status})"
       else
         "No success information for proposal: #{barclamp}-#{name} (status #{status})"
       end
       out = s.split(":")
       out[0] = I18n.t out[0], :default=> out[0]
       return out.join(":").to_s
    end
    
    def description
      item['description']
    end
    
    def elements
      item.raw_data['deployment'][self.barclamp]["elements"]
    end

    def all_elements
      item.raw_data['deployment'][self.barclamp]["element_order"].flatten.uniq
    end

    def role
      RoleObject.find_role_by_name("#{barclamp}-config-#{name}")
    end

    def crowbar_revision
      item["deployment"][barclamp]["crowbar-revision"].to_i rescue 0
    end

    def latest_applied?
      item["deployment"][barclamp]["crowbar-applied"] rescue false
    end

    def latest_applied=(applied)
      item["deployment"] ||= {}
      item["deployment"][barclamp] ||= {}
      item["deployment"][barclamp]["crowbar-applied"] = applied
    end

    def active?
      !role.nil?
    end

    def raw_data
      item.raw_data
    end

    def raw_data=(value)
      item.raw_data = value
    end

    def [](attrib)
      item[attrib]
    end

    def []=(attrib, value)
      item[attrib] = value
    end
  end
end
