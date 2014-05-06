require "spec_helper"

describe ServiceObject do
  let(:service_object) { so = ServiceObject.new(Logger.new("/dev/null")); so.bc_name = "crowbar"; so }
  let(:proposal) { ProposalObject.find_proposal("crowbar", "default") }
  let(:proposal_elements) {
    [
      ["crowbar", ["admin"]            ],
      ["dns",     ["admin", "testing"] ],
    ]}

  describe "validate_proposal_elements" do
    it "raises on duplicate nodes" do
      pe = proposal_elements
      pe.first.last.push("admin")

      expect {
        service_object.validate_proposal_elements(pe)
      }.to raise_error(/#{Regexp.escape(I18n.t('proposal.failures.duplicate_elements_in_role'))}/)
    end

    it "raises on missing nodes" do
      pe = proposal_elements
      pe.first.last.push("missing")

      expect {
        service_object.validate_proposal_elements(pe)
      }.to raise_error(/#{Regexp.escape(I18n.t('proposal.failures.unknown_node'))}/)
    end
  end

  describe "validate_proposal" do
    it "raises ValidationFailed on missing schema" do
      service_object.stubs(:proposal_schema_directory).returns("/idontexist")
      expect {
        service_object.validate_proposal(proposal.raw_data)
      }.to raise_error(Chef::Exceptions::ValidationFailed)
    end

    it "validates the proposal" do
      prop = proposal
      CrowbarValidator.any_instance.expects(:validate).with(prop.raw_data).once.returns([])
      service_object.validate_proposal(prop.raw_data)
    end

    it "raises ValidationFailed on invalid proposal" do
      prop = proposal
      expect {
        service_object.validate_proposal(prop.raw_data)
      }.to raise_error(Chef::Exceptions::ValidationFailed)
    end

    it "leaves empty validation errors" do
      prop = proposal
      prop.raw_data['attributes']['crowbar'].delete('barclamps')
      prop.raw_data['attributes']['crowbar'].delete('run_order')

      service_object.validate_proposal(prop.raw_data)
      service_object.instance_variable_get(:@validation_errors).should be_empty
    end
  end

  describe "validate proposal constraints" do
    let(:dns_proposal) { ProposalObject.find_proposal("dns", "default") }
    let(:dns_service)  { so = ServiceObject.new(Logger.new("/dev/null")); so.bc_name = "dns"; so }

    describe "count" do
      it "limits the number of elements in a role" do
        dns_service.stubs(:role_constraints).returns({ "dns-client" => { "count" => 0, "admin" => true } })
        dns_service.validate_proposal_constraints(dns_proposal)
        dns_service.validation_errors.first.should match(/accept up to 0 elements only/)
      end
    end

    describe "admin" do
      it "does not allow admin nodes to be assigned by default" do
        dns_service.stubs(:role_constraints).returns({ "dns-client" => { } })
        dns_service.validate_proposal_constraints(dns_proposal)
        dns_service.validation_errors.first.should match(/does not accept admin nodes/)
      end
    end

    describe "unique" do
      it "limits the number of roles for an element to one" do
        dns_service.stubs(:role_constraints).returns({ "dns-client" => { "unique" => true, "admin" => true } })
        dns_service.validate_proposal_constraints(dns_proposal)
        dns_service.validation_errors.first.should match(/cannot be assigned to another role/)
      end
    end

    describe "cluster" do
      it "does not allow clusters of nodes to be assigned" do
        dns_service.stubs(:role_constraints).returns({ "dns-client" => { "cluster" => false, "admin" => true } })
        dns_service.stubs(:is_cluster?).returns(true)
        dns_service.validate_proposal_constraints(dns_proposal)
        dns_service.validation_errors.first.should match(/does not accept clusters/)
      end
    end

    describe "conflicts_with" do
      it "does not allow a node to be assigned to conflicting roles" do
        dns_service.stubs(:role_constraints).returns({
          "dns-server" => { "conflicts_with" => ["dns-client", "hawk-server"], "admin" => true },
          "dns-client" => { "conflicts_with" => ["dns-server", "hawk-server"], "admin" => true },
        })

        dns_service.validate_proposal_constraints(dns_proposal)
        dns_service.validation_errors.first.should match(/cannot be assigned to both role/)
      end
    end
  end
end

