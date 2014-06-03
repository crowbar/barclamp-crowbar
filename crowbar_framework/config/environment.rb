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

RAILS_GEM_VERSION = "2.3.17" unless defined? RAILS_GEM_VERSION
require File.expand_path("../boot", __FILE__)

Rails::Initializer.run do |config|
  unless AppConfig[:use_bundler]
    config.gem "app_config", :version => "1.0.2"
    config.gem "chef", :version => "10.24.4"
    config.gem "haml", :version => "3.1.6"
    config.gem "hike", :version => "1.2.1"
    config.gem "i18n", :version => "0.4.2"
    config.gem "json", :version => "1.6.1"
    config.gem "kwalify", :version => "0.7.2"
    config.gem "multi_json", :version => "1.0.3"
    config.gem "rack", :version => "1.1.6"
    config.gem "sass", :version => "3.2.12"
    config.gem "simple-navigation", :version => "3.7.0"
    config.gem "sprockets", :version => "2.10.1"
    config.gem "sprockets-sass", :version => "1.0.2"
    config.gem "sprockets-helpers", :version => "1.1.0"
    config.gem "sqlite3", :version => "1.3.6"
    config.gem "syslogger", :version => "1.3.0"
    config.gem "tilt", :version => "1.3.3"
    config.gem "mime-types", :version => "1.18", :lib => "mime/types"
  end

  config.i18n.default_locale = :en

  config.i18n.load_path += Dir[
    Rails.root.join("config", "locales", "**", "*.{rb,yml}").to_s
  ]

  config.time_zone = "UTC"

  unless defined?(CROWBAR_LOG_DIR)
    if RAILS_ENV == "test"
      CROWBAR_LOG_DIR = "#{RAILS_ROOT}/log"
    else
      CROWBAR_LOG_DIR = "/var/log/crowbar"
    end
  end

  config.logger = Logger.new("#{CROWBAR_LOG_DIR}/#{RAILS_ENV}.log")
  config.logger.formatter = Logger::Formatter.new

  CHEF_CLIENT_KEY = "/opt/dell/crowbar_framework/config/client.pem" unless defined? CHEF_CLIENT_KEY
  CHEF_NODE_NAME ="crowbar" unless defined? CHEF_NODE_NAME
  CHEF_SERVER_URL = "http://localhost:4000" unless defined? CHEF_SERVER_URL
  CROWBAR_VERSION = "0.0.1" unless defined? CROWBAR_VERSION

  CONVERGED_ADMIN = true # flag indicating at we can assume all Crowbar services on a single server
  HAVE_CHEF_WEBUI = true # flag indicating whether it's okay to link to the chef webui
  SERVER_PID = %x[ps ax | grep "rainbows master" | grep -v grep].split(" ")[0]
end
