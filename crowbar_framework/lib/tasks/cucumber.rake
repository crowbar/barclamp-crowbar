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

unless Rails.env.production?
  vendored_cucumber_bin = Dir[
    Rails.root.join(
      "vendor", 
      "{gems,plugins}", 
      "cucumber*", 
      "bin", 
      "cucumber"
    )
  ].first

  $LOAD_PATH.unshift(
    File.join(File.dirname(vendored_cucumber_bin), "..", "lib")
  ) unless vendored_cucumber_bin.nil?

  begin
    require "cucumber/rake/task"

    namespace :cucumber do
      Cucumber::Rake::Task.new({ ok: "test:prepare" }, "Run features that should pass") do |t|
        t.binary = vendored_cucumber_bin 
        t.fork = true
        t.profile = "default"
      end

      Cucumber::Rake::Task.new({ wip: "test:prepare" }, "Run features that are being worked on") do |t|
        t.binary = vendored_cucumber_bin
        t.fork = true
        t.profile = "wip"
      end

      Cucumber::Rake::Task.new({ rerun: "test:prepare" }, "Record failing features and run only them if any exist") do |t|
        t.binary = vendored_cucumber_bin
        t.fork = true
        t.profile = "rerun"
      end

      desc "Run all features"
      task all: [:ok, :wip]

      task :statsetup do
        require "rails/code_statistics"

        ::STATS_DIRECTORIES << %w(Cucumber\ features features) if File.exist?("features")
        ::CodeStatistics::TEST_TYPES << "Cucumber features" if File.exist?("features")
      end
    end

    desc "Alias for cucumber:ok"
    task cucumber: "cucumber:ok"

    task default: :cucumber

    task "test:prepare" do
    end

    task stats: "cucumber:statsetup"
  rescue LoadError
    desc "Cucumber rake task not available"
    task :cucumber do
      abort "Cucumber rake task is not available"
    end
  end
end
