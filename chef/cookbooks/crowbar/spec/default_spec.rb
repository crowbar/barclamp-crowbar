require_relative 'spec_helper'

describe 'crowbar::default' do
  describe 'suse' do
    let(:runner) { ChefSpec::Runner.new(SUSE_OPTS) }
    let(:node) { runner.node }
    let(:chef_run) { runner.converge(described_recipe) }
    before do
      # XXX test these
      stub_command("egrep -qi '^crowbar:' /etc/passwd").and_return true
      stub_command("export HOME=/root;knife client list -u crowbar -k /opt/dell/crowbar_framework/config/client.pem").and_return true
      stub_command("ls -al /opt/dell/crowbar_framework/README | grep -q crowbar").and_return true
      stub_command("test -L /usr/sbin/rccrowbar").and_return true
      stub_command("/sbin/chkconfig crowbar | grep -q on").and_return false
      # XXX this is a bit hacky since there are in fact two different
      # calls to the same (:open) method, but I couldn't figure out a
      # way to mock the same method twice for different parameters and
      # with different return values
      allow_any_instance_of(Kernel).to receive(:open)
        .and_return double('/chef', :read => <<-HERE)
     {"disk_size": 100000001,
      "view_index": {"disk_size": 100000001}
     }
HERE
    end

    it 'enables the crowbar-webserver service' do
      expect(chef_run).to run_bash('Enable crowbar service')
        .with_code('/sbin/chkconfig crowbar on')
    end
  end
end
