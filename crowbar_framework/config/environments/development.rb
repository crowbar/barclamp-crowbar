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

Crowbar::Application.configure do
  config.cache_classes = false
  config.eager_load = false
  config.consider_all_requests_local = true

  config.log_level = :debug

  config.i18n.fallbacks = false

  config.serve_static_assets = true
  config.assets.debug = true
  config.assets.js_compressor = :uglifier
  config.assets.css_compressor = :sass
  config.assets.compile = true
  config.assets.digest = false
  config.assets.version = "1.0"

  config.action_dispatch.show_exceptions = true
  config.action_controller.perform_caching = false
  config.action_controller.allow_forgery_protection = true
  config.action_mailer.raise_delivery_errors = false
  config.active_support.deprecation = :log
  config.active_record.migration_error = :page_load

  config.logger = ActiveSupport::TaggedLogging.new(
    Logger.new Rails.root.join("log", "development.log") # SyslogLogger.new
  )

  #config.logger.formatter = ::Logger::Formatter.new
end

