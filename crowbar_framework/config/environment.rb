# Copyright 2011-2013, Dell
# Copyright 2013, SUSE LINUX Products GmbH
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# Author: Rob Hirschfeld
# Author: SUSE LINUX Products GmbH
#

RAILS_GEM_VERSION = "2.3.17" unless defined? RAILS_GEM_VERSION
require File.expand_path("../boot", __FILE__)

Rails::Initializer.run do |config|
  unless AppConfig[:use_bundler]
    config.gem "haml"
    config.gem "sass"
    config.gem "simple-navigation"
    config.gem "i18n"
    config.gem "json"
    config.gem "sprockets-sass"
    config.gem "sprockets-helpers"
  end

  config.time_zone = "UTC"

  CROWBAR_LOG_DIR = "/var/log/crowbar" unless defined? CROWBAR_LOG_DIR

  config.logger = Logger.new("/var/log/crowbar/#{RAILS_ENV}.log")
  config.logger.formatter = Logger::Formatter.new

  CHEF_CLIENT_KEY = "/opt/dell/crowbar_framework/config/client.pem" unless defined? CHEF_CLIENT_KEY
  CHEF_NODE_NAME ="crowbar" unless defined? CHEF_NODE_NAME
  CHEF_SERVER_URL = "http://localhost:4000" unless defined? CHEF_SERVER_URL
  CHEF_ONLINE = true unless defined? CHEF_ONLINE
  OFFLINE_FILES_DIR = "db" unless defined? OFFLINE_FILES_DIR
  CROWBAR_VERSION = "0.0.1" unless defined? CROWBAR_VERSION

  CONVERGED_ADMIN = true # flag indicating at we can assume all Crowbar services on a single server
  HAVE_CHEF_WEBUI = true # flag indicating whether it's okay to link to the chef webui
  SERVER_PID = %x[ps ax | grep "rainbows master" | grep -v grep].split(" ")[0]
end
