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

# In the development environment your application's code is reloaded on
# every request. This slows down response time but is perfect for development
# since you don't have to restart the webserver when you make code changes.
config.cache_classes = false

# Full error reports are enabled and caching is turned off
config.action_controller.consider_all_requests_local = true
config.action_controller.perform_caching = false
config.action_view.cache_template_loading = false
config.action_view.debug_rjs = true

# Log error messages when you accidentally call methods on nil.
config.whiny_nils = true

# Set a verbose log level to get the informations we need
config.log_level = :debug

# Don't care if the mailer can't send
config.action_mailer.raise_delivery_errors = false

# Enable request forgery protection in development environment
config.action_controller.allow_forgery_protection = true

# Enable threaded mode
#config.threadsafe!

CHEF_CLIENT_KEY = "/opt/dell/crowbar_framework/config/client.pem"
CHEF_NODE_NAME ="crowbar"
CHEF_SERVER_URL = "http://localhost:4000"
CROWBAR_VERSION = "Development"
