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

ENV["BUNDLE_GEMFILE"] ||= File.expand_path("../../Gemfile", __FILE__)

require "uri"
require "net/http"

if File.exists? ENV["BUNDLE_GEMFILE"]
  require "bundler/setup"
  require "rails/all"
  require "sprockets/railtie"

  Bundler.require(:default, Rails.env)
else
  #
  # WARNING, this content between these markers gets replaced by the rake 
  # task crowbar:generate:dependencies! Don't update it manually!
  #

  # RAILSINCLUDE START
  gem "rails", version: "4.1.0"
  require "rails/all"
  # RAILSINCLUDE END

  #
  # WARNING, this content between these markers gets replaced by the rake 
  # task crowbar:generate:dependencies! Don't update it manually!
  #

  # DEPENDENCIES START
  gem "dotenv-rails", version: "~> 0.10.0"
  require "dotenv-rails"

  gem "haml-rails", version: "~> 0.5.3"
  require "haml-rails"

  gem "sass-rails", version: "~> 4.0.3"
  require "sass-rails"

  gem "pry-rails", version: "~> 0.3.2"
  require "pry-rails"

  gem "rack", version: "~> 1.5.2"
  require "rack"

  gem "bcrypt", version: "~> 3.1.7"
  require "bcrypt"

  gem "kwalify", version: "~> 0.7.2"
  require "kwalify"

  gem "simple-navigation", version: "~> 3.12.2"
  require "simple-navigation"

  gem "simple_navigation_renderers", version: "~> 1.0.2"
  require "simple_navigation_renderers"

  gem "sqlite3", version: "~> 1.3.9"
  require "sqlite3"

  gem "gli", version: "~> 2.9.0"
  require "gli"

  gem "cocaine", version: "~> 0.5.4"
  require "cocaine"

  gem "hashie", version: "~> 2.0.5"
  require "hashie"

  gem "delayed_job_active_record", version: "~> 4.0.1"
  require "delayed_job_active_record"

  gem "rainbows-rails", version: "~> 1.0.1"
  require "rainbows-rails"

  gem "redcarpet", version: "~> 3.1.1"
  require "redcarpet"

  gem "nokogiri", version: "~> 1.6.1"
  require "nokogiri"

  gem "bootstrap-sass", version: "~> 3.1.1"
  require "bootstrap-sass"

  gem "font-awesome-rails", version: "~> 4.0.3"
  require "font-awesome-rails"

  gem "closure-compiler", version: "~> 1.1.10"
  require "closure-compiler"

  gem "cells", version: "~> 3.10.0"
  require "cells"

  gem "js-routes", version: "~> 0.9.7"
  require "js-routes"

  gem "json", version: "~> 1.8.1"
  require "json"

  gem "activeresource", version: "~> 4.0.0"
  require "active_resource"

  gem "activerecord-session_store", version: "~> 0.1.0"
  require "activerecord/session_store"

  gem "chef", version: "~> 10.32.2"
  require "chef"
  # DEPENDENCIES END

  require "sprockets/railtie"
end
