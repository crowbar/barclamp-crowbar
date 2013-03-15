# Load the rails application
require File.expand_path('../application', __FILE__)

if File.exists?("/opt/dell/barclamps/deployer/chef/cookbooks/barclamp/libraries")
  require "/opt/dell/barclamps/deployer/chef/cookbooks/barclamp/libraries/ip.rb"
  require "/opt/dell/barclamps/deployer/chef/cookbooks/barclamp/libraries/nic.rb"
  require "/opt/dell/barclamps/deployer/chef/cookbooks/barclamp/libraries/nethelper.rb"
end

# Initialize the rails application
Crowbar::Application.initialize!
