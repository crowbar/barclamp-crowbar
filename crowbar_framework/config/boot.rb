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

ENV["BUNDLE_GEMFILE"] ||= File.expand_path("../../Gemfile", __FILE__)

require "uri"
require "net/http"

if File.exists? ENV["BUNDLE_GEMFILE"]
  require "bundler/setup"
  require "rails/all"

  Bundler.require(:default, Rails.env)
else
  # rails related
  gem "rails", version: "~> 4.1.9"
  require "rails/all"

  gem "haml-rails", version: "~> 0.5.3"
  require "haml-rails"

  gem "sass-rails", version: "~> 4.0.5"
  require "sass-rails"

  gem "rainbows-rails", version: "~> 1.0.1"
  require "rainbows-rails"

  # general stuff
  gem "activerecord-session_store", version: "~> 0.1.0"
  require "activerecord/session_store"

  gem "active_model_serializers", version: "~> 0.9.0"
  require "active_model_serializers"

  gem "activeresource", version: "~> 4.0.0"
  require "active_resource"

  gem "closure-compiler", version: "~> 1.1.10"
  require "closure-compiler"

  gem "dotenv", version: "~> 1.0.2"
  require "dotenv"

  gem "hashie", version: "~> 3.3.1"
  require "hashie"

  gem "i18n-js", version: "~> 2.1.2"
  require "i18n-js"

  gem "js-routes", version: "~> 0.9.7"
  require "js-routes"

  gem "kwalify", version: "~> 0.7.2"
  require "kwalify"

  gem "mime-types", version: "~> 1.25.1"
  require "mime/types"

  gem "redcarpet", version: "~> 3.2.0"
  require "redcarpet"

  gem "simple-navigation", version: "~> 3.12.2"
  require "simple-navigation"

  gem "simple_navigation_renderers", version: "~> 1.0.2"
  require "simple_navigation_renderers"

  gem "sqlite3", version: "~> 1.3.9"
  require "sqlite3"

  gem "syslogger", version: "~> 1.6.0"
  require "syslogger"

  # chef related
  gem "mixlib-shellout", version: "~> 1.4.0"
  require "mixlib/shellout"

  gem "ohai", version: "~> 6.24"
  require "ohai"

  gem "chef", version: "~> 10.32.2"
  require "chef"

  # without require
  gem "dotenv-deployment", version: "~> 0.2.0"
end
