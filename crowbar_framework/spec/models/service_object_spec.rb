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
end
