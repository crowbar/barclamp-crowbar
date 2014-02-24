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

Bundler.require(:default, Rails.env)

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

