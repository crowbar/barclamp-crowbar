# -*- encoding : utf-8 -*-
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
  config.cache_classes = true
  config.eager_load = true
  config.consider_all_requests_local = false

  config.log_level = :info

  config.i18n.fallbacks = true

  config.serve_static_assets = false
  config.assets.debug = false
  config.assets.js_compressor = :closure
  config.assets.css_compressor = :sass
  config.assets.compile = true
  config.assets.digest = true
  config.assets.version = "1.0"

  config.action_dispatch.show_exceptions = false
  config.action_controller.perform_caching = true
  config.action_controller.allow_forgery_protection = true
  config.action_mailer.raise_delivery_errors = false
  config.active_support.deprecation = :notify
  config.active_record.migration_error = :page_load

  config.logger = ActiveSupport::TaggedLogging.new(
    Logger.new File.join(ENV["CROWBAR_LOG_DIR"], "production.log") # SyslogLogger.new
  )

  #config.logger.formatter = ::Logger::Formatter.new
end
