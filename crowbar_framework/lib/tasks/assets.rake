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

require "fileutils"
require "pathname"

namespace :assets do
  def ruby_rake_task(task, fork = true)
    env = ENV["RAILS_ENV"] || "production"
    groups = ENV["RAILS_GROUPS"] || "assets"
    args = [$0, task, "RAILS_ENV=" + env, "RAILS_GROUPS=" + groups]
    args << "--trace" if Rake.application.options.trace

    if $0 =~ /rake\.bat\Z/i
      Kernel.exec $0, *args
    else
      fork ? ruby(*args) : Kernel.exec(FileUtils::RUBY, *args)
    end
  end

  def invoke_or_reboot_rake_task(task)
    if ENV["RAILS_GROUPS"].to_s.empty? || ENV["RAILS_ENV"].to_s.empty?
      ruby_rake_task task
    else
      Rake::Task[task].invoke
    end
  end

  desc "Compile all the assets named in config.assets.precompile"
  task :precompile do
    invoke_or_reboot_rake_task "assets:precompile:all"
  end

  namespace :precompile do
    def internal_precompile(digest = nil)
      _ = ActionView::Base

      sprockets = Sprockets.env
      manifest_path = Pathname.new(Rails.public_path).join("assets", "manifest.json")

      manifest = Sprockets.manifest
      manifest.compile
    end

    task :all do
      Rake::Task["assets:precompile:primary"].invoke
    end

    task :primary => ["assets:environment", "tmp:cache:clear"] do
      internal_precompile
    end

    task :nondigest => ["assets:environment", "tmp:cache:clear"] do
      internal_precompile(false)
    end
  end

  desc "Remove compiled assets"
  task :clean do
    invoke_or_reboot_rake_task "assets:clean:all"
  end

  namespace :clean do
    task :all => ["assets:environment", "tmp:cache:clear"] do
      public_asset_path = Pathname.new(Rails.public_path).join("assets")
      rm_rf public_asset_path, :secure => true
    end
  end

  task :environment do
    Rake::Task["environment"].invoke
  end
end
