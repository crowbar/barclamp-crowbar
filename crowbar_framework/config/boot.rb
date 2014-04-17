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

gem "rails", version: "4.0.4"

require "rails/all"
require "sprockets/railtie"
require "net/http"

requirements = {}.tap do |deps|
  deps["dotenv-rails"] = { version: "~> 0.10.0" }
  deps["json"] = { version: "1.7.7" }
  deps["rack"] = { version: "~> 1.5.2" }
  deps["haml-rails"] = { version: "~> 0.5.3" }
  deps["sass-rails"] = { version: "~> 4.0.2" }
  deps["bcrypt"] = { version: "~> 3.1.7" }
  deps["kwalify"] = { version: "~> 0.7.2" }
  deps["simple-navigation"] = { version: "~> 3.12.2" }
  deps["simple_navigation_renderers"] = { version: "~> 1.0.2" }
  deps["sqlite3"] = { version: "~> 1.3.9" }
  deps["gli"] = { version: "~> 2.9.0" }
  deps["cocaine"] = { version: "~> 0.5.3" }
  deps["hashie"] = { version: "~> 2.0.5" }
  deps["delayed_job_active_record"] = { version: "~> 4.0.0" }
  deps["faye"] = { version: "~> 1.0.1" }
  deps["thin"] = { version: "~> 1.6.2" }
  deps["redcarpet"] = { version: "~> 3.1.1" }
  deps["nokogiri"] = { version: "~> 1.6.1" }
  deps["bootstrap-sass"] = { version: "~> 3.1.1" }
  deps["font-awesome-rails"] = { version: "~> 4.0.3" }
  deps["closure-compiler"] = { version: "~> 1.1.10" }
  deps["js-routes"] = { version: "~> 0.9.7" }
  deps["chef"] = { version: "~> 10.24.4" }

  deps["activeresource"] = { 
    version: "~> 4.0.0",
    require: "active_resource" 
  }

  deps["activerecord-session_store"] = { 
    version: "~> 0.1.0", 
    require: "activerecord/session_store" 
  }

  case Rails.env.to_sym
  when :test
    deps["rspec-rails"] = { version: "~> 2.14.1" }
    deps["mocha"] = { version: "~> 1.0.0" }
    deps["webmock"] = { version: "~> 1.17.4" }
    deps["sinatra"] = { version: "~> 1.4.4" }

    deps["cucumber-rails"] = { 
      version: "~> 1.4.0", 
      require: false 
    }

    deps["simplecov"] = { 
      version: "~> 0.8.2", 
      require: false 
    }
  when :development
    #deps["debugger"] = { version: "~> 1.6.6" }
    #deps["byebug"] = { version: "~> 2.7.0" }

    deps["rspec-rails"] = { version: "~> 2.14.1" }
    deps["guard"] = { version: "~> 2.5.1" }
    deps["better_errors"] = { version: "~> 1.1.0" }
    deps["meta_request"] = { version: "~> 0.2.8" }
    deps["quiet_assets"] = { version: "~> 1.0.2" }
    deps["pry-rails"] = { version: "~> 0.3.2" }

    deps["brakeman"] = { 
      version: "~> 2.4.3", 
      require: false 
    }

    deps["rubocop"] = { 
      version: "~> 0.18.1", 
      require: false 
    }

    case RUBY_ENGINE
    when "ruby"
      deps["binding_of_caller"] = { version: "~> 0.7.2" }
    end
  end
end

requirements.each do |name, options|
  required = options.delete(:require) || true
  gem name, options

  if required == true
    require name
  else
    if required.is_a? String
      require required
    end
  end
end
