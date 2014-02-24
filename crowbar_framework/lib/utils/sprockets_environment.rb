# -*- encoding : utf-8 -*-
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
# Author: Dell Crowbar Team
# Author: SUSE LINUX Products GmbH
#

module Utils
  module SprocketsEnvironment
    class << self
      def env
        @env ||= begin
          sprockets = Sprockets::Environment.new

          sprockets.append_path "app/assets/javascripts"
          sprockets.append_path "app/assets/stylesheets"
          sprockets.append_path "app/assets/fonts"
          sprockets.append_path "app/assets/images"

          if Rails.env.production?
            sprockets.css_compressor = :sass
          end

          Sprockets::Helpers.configure do |config|
            config.environment = sprockets
            config.prefix = "/assets"
            config.digest = false

            if Rails.env.production?
              config.debug = false
            else
              config.debug = true
            end
          end

          sprockets
        end
      end
    end
  end
end

