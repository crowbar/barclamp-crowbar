# Set global constants
#
# It's generally bad practice to use globals, but let's keep them for now to
# avoid changing too many things at once. We'll move them to the appropriate
# models later so that they don't pollute the global namespace.

# CROWBAR_VERSION is defined in config/environments/production.rb. Fallback
# here for all other environments.
defined?(CROWBAR_VERSION) || CROWBAR_VERSION = 'Development'

CHEF_CLIENT_KEY = '/opt/dell/crowbar_framework/config/client.pem'
CHEF_NODE_NAME  = 'crowbar'
CHEF_SERVER_URL = 'http://localhost:4000'

# Fallback to nil when the PID file does not exist. Eg. when Crowbar server is
# not running and we're loading the environment from Rake or Rails console.
pid_file   = 'tmp/pids/server.pid'
SERVER_PID = File.exists?(pid_file) ? File.read(pid_file) : nil
