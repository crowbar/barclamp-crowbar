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

# The test environment is used exclusively to run your application's
# test suite.  You never need to work with it otherwise.  Remember that
# your test database is "scratch space" for the test suite and is wiped
# and recreated between test runs.  Don't rely on the data there!
config.cache_classes = true

# Full error reports are disabled and caching is turned on
config.action_controller.consider_all_requests_local = true
config.action_controller.perform_caching = false
config.action_view.cache_template_loading = true
config.action_view.debug_rjs = false

# Log error messages when you accidentally call methods on nil.
config.whiny_nils = true

# Set a verbose log level to get the informations we need
config.log_level = :debug

# Tell Action Mailer not to deliver emails to the real world.
# The :test delivery method accumulates sent emails in the
# ActionMailer::Base.deliveries array.
config.action_mailer.delivery_method = :test

# Disable request forgery protection in test environment
config.action_controller.allow_forgery_protection = false

# Enable threaded mode
#config.threadsafe!

CHEF_CLIENT_KEY = "/opt/dell/crowbar_framework/config/client.pem"
CHEF_NODE_NAME ="crowbar"
CHEF_SERVER_URL = "http://localhost:4000"
CROWBAR_VERSION = "Test"
