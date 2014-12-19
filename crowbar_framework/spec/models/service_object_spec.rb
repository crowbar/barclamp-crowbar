#
# Copyright 2011-2013, Dell
# Copyright 2013-2014, SUSE LINUX Products GmbH
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

require "spec_helper"

describe ServiceObject do
  let(:service_object) { so = ServiceObject.new(Logger.new("/dev/null")); so.bc_name = "crowbar"; so }
  let(:proposal) { ProposalObject.find_proposal("crowbar", "default") }
  let(:proposal_elements) {
    [
      ["crowbar", ["admin"]            ],
      ["dns",     ["admin", "testing"] ],
    ]}

  describe "service object" do
    it "responds to include cluster method" do
      service_object.should respond_to(:available_clusters)
    end
  end

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

  describe "save the state of a node" do
    node_name = "d00-0c-29-80-2f-68.crowbar.com"

    it "should save the state of a node" do
      nodes_to_save = { NodeObject.find_node_by_name(node_name) => true }
      service_object.save_node_state(nodes_to_save, "calling_method").should be_true
    end

    it "should not save the state of a node" do
      nodes_to_save = { NodeObject.find_node_by_name(node_name) => false }
      service_object.save_node_state(nodes_to_save, "calling_method").should be_false
    end
  end

  describe "roles suffixed with _remove" do
    node_name = "d00-0c-29-80-2f-68.crowbar.com"

    describe "_remove role in the runlist" do
      it "should delete the _remove role from the runlist" do
        pending_node_actions = { node_name => { :remove => [], :add => ["heat-server_remove"] } }
        nodes_to_save = service_object.delete_remove_role_from_runlist(pending_node_actions)
        nodes_to_save.should be_an_instance_of(Hash)
        nodes_to_save.each do |node, save_it|
          node.should be_an_instance_of(NodeObject)
          save_it.should be_true
        end
      end
    end

    describe "no _remove role in the runlist" do
      it "should not change the node when there is no _remove role" do
        pending_node_actions = { node_name => { :remove => [], :add => ["heat-server"] } }
        nodes_to_save = service_object.delete_remove_role_from_runlist(pending_node_actions)
        nodes_to_save.should be_an_instance_of(Hash)
        nodes_to_save.each do |node, save_it|
          node.should be_an_instance_of(NodeObject)
          save_it.should be_false
        end
      end
    end
  end
end
