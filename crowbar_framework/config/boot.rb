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

require "thread"
require "rubygems"
require "pathname"
require "app_config"

RAILS_ROOT = File.expand_path("../..", __FILE__) unless defined?(RAILS_ROOT)

module Rails
  class << self
    def root
      ::Pathname.new RAILS_ROOT
    end

    def boot!
      unless booted?
        preinitialize
        pick_boot.run
      end
    end

    def booted?
      defined? Rails::Initializer
    end

    def pick_boot
      (vendor_rails? ? VendorBoot : GemBoot).new
    end

    def vendor_rails?
      self.root.join("vendor", "rails").directory?
    end

    def preinitialize
      load(preinitializer_path) if preinitializer_path.file?
    end

    def preinitializer_path
      self.root.join("config", "preinitializer.rb")
    end
  end

  class Boot
    def run
      load_initializer
      Rails::Initializer.run(:set_load_path)
    end
  end

  class VendorBoot < Boot
    def load_initializer
      require Rails.root.join("vendor", "rails", "railties", "lib", "initializer")

      Rails::Initializer.run(:install_gem_spec_stubs)
      Rails::GemDependency.add_frozen_gem_path
    end
  end

  class GemBoot < Boot
    def load_initializer
      self.class.load_rubygems
      load_rails_gem

      require "initializer"
    end

    def load_rails_gem
      if version = self.class.gem_version
        gem "rails", version
      else
        gem "rails"
      end
    rescue Gem::LoadError => load_error
      $stderr.puts %(Missing the Rails #{version} gem. Please `gem install -v=#{version} rails`, update your RAILS_GEM_VERSION setting in config/environment.rb for the Rails version you do have installed, or comment out RAILS_GEM_VERSION to use the latest version installed.)
      exit 1
    end

    class << self
      def rubygems_version
        Gem::RubyGemsVersion rescue nil
      end

      def gem_version
        if defined? RAILS_GEM_VERSION
          RAILS_GEM_VERSION
        elsif ENV.include?("RAILS_GEM_VERSION")
          ENV["RAILS_GEM_VERSION"]
        else
          parse_gem_version(read_environment_rb)
        end
      end

      def load_rubygems
        min_version = "1.3.2"

        unless rubygems_version >= min_version
          $stderr.puts %Q(Rails requires RubyGems >= #{min_version} (you have #{rubygems_version}). Please `gem update --system` and try again.)
          exit 1
        end
      rescue LoadError
        $stderr.puts %Q(Rails requires RubyGems >= #{min_version}. Please install RubyGems and try again: http://rubygems.rubyforge.org)
        exit 1
      end

      def parse_gem_version(text)
        $1 if text =~ /^[^#]*RAILS_GEM_VERSION\s*=\s*["']([!~<>=]*\s*[\d.]+)["']/
      end

      private
        def read_environment_rb
          File.read(Rails.root.join("config", "environment.rb"))
        end
    end
  end
end

AppConfig.setup(
  :yaml => Rails.root.join("config", "app_config.yml")
)

if AppConfig[:use_bundler]
  class Rails::Boot
    def run
      load_initializer

      Rails::Initializer.class_eval do
        def load_gems
          @bundler_loaded ||= Bundler.require :default, Rails.env
        end
      end

      Rails::Initializer.run(:set_load_path)
    end
  end
end

Rails.boot!
