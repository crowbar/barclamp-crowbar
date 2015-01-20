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

Rails.application.configure do
  config.cache_classes = true
  config.eager_load = false
  config.consider_all_requests_local = true
  config.serve_static_files = true
  config.force_ssl = false
  config.autoflush_log = false

  config.action_dispatch.show_exceptions = false
  config.action_dispatch.cookies_serializer = :json

  config.action_controller.perform_caching = false
  config.action_controller.allow_forgery_protection = false

  config.action_mailer.raise_delivery_errors = false
  config.action_mailer.delivery_method = :test

  config.action_view.raise_on_missing_translations = true

  config.active_support.deprecation = :stderr

  config.active_record.migration_error = :page_load
  config.active_record.dump_schema_after_migration = false

  config.assets.debug = false
  config.assets.raise_runtime_errors = true
  config.assets.js_compressor = :uglifier
  config.assets.css_compressor = :sass
  config.assets.compile = false
  config.assets.digest = true

  config.i18n.fallbacks = true

  config.log_level = :debug
  config.log_tags = []

  config.logger = ActiveSupport::TaggedLogging.new(
    Logger.new(Rails.root.join("log", "test.log"))
  )
  config.logger.formatter = ::Crowbar::Logger::Formatter.new
end
