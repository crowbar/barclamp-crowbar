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

require File.expand_path("../boot", __FILE__)
require "rails/all"
require "sprockets/railtie"

if ENV["USE_BUNDLER"]
  Bundler.require(:default, Rails.env)
else
  #
  # This needs to be in sync with the Gemfile. First we activate 
  # specific versions for the gems, and afterwards we require all 
  # gems. That's the best workaround for the usage of bundler.
  #

  gem "json", version: "1.7.7"
  gem "rack", version: "~> 1.5.2"
  gem "dotenv-rails", version: "~> 0.9.0"
  gem "haml-rails", version: "~> 0.5.3"
  gem "sass-rails", version: "~> 4.0.1"
  gem "jbuilder", version: "~> 2.0.3"
  gem "bcrypt", version: "~> 3.1.2"
  gem "kwalify", version: "~> 0.7.2"
  gem "simple-navigation", version: "~> 3.11.0"
  gem "sqlite3", version: "~> 1.3.8"
  gem "gli", version: "~> 2.9.0"
  gem "cocaine", version: "~> 0.5.3"
  gem "hashie", version: "~> 2.0.5"
  gem "activerecord-session_store", version: "~> 0.1.0"
  gem "chef", version: "~> 10.30.4"

  gem "uglifier", version: "~> 2.2.1"
  gem "therubyracer", version: "~> 0.12.1"

  require "json"
  require "rack"
  require "dotenv-rails"
  require "haml-rails"
  require "sass-rails"
  require "jbuilder"
  require "bcrypt"
  require "kwalify"
  require "simple-navigation"
  require "sqlite3"
  require "gli"
  require "cocaine"
  require "hashie"
  require "chef"
  require "uglifier"
  require "therubyracer"
  require "activerecord/session_store"
end

module Crowbar
  class Application < Rails::Application
    SERVER_PID = %x[ps ax | grep "rainbows master" | grep -v grep].split(" ")[0]

    config.autoload_paths += %W(
      #{config.root}/lib
    )

    config.time_zone = "UTC"

    config.i18n.enforce_available_locales = true
    config.i18n.default_locale = :en

    config.i18n.load_path += Dir[
      Rails.root.join("config", "locales", "**", "*.{rb,yml}").to_s
    ]
  end
end
