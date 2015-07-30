#
# Copyright 2011-2013, Dell
# Copyright 2013-2015, SUSE Linux GmbH
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

unless ARGV.any? { |a| a =~ /^gems/ }
  begin
    require "cucumber/rake/task"

    namespace :cucumber do
      Cucumber::Rake::Task.new(
        { ok: "test:prepare" },
        "Run features that should pass"
      ) do |t|
        t.fork = true
        t.profile = "default"
      end

      Cucumber::Rake::Task.new(
        { :wip => "test:prepare" },
        "Run features that are being worked on"
      ) do |t|
        t.fork = true
        t.profile = "wip"
      end

      Cucumber::Rake::Task.new(
        { :rerun => "test:prepare" },
        "Record failing features and run only them if any exist"
      ) do |t|
        t.fork = true
        t.profile = "rerun"
      end

      desc "Run all features"
      task all: [:ok, :wip]

      task :statsetup do
        require "rails/code_statistics"

        if File.exist? "features"
          ::STATS_DIRECTORIES.push ["features"]
          ::CodeStatistics::TEST_TYPES.push "Cucumber"
        end
      end
    end

    task "test:prepare" do
    end

    task cucumber: "cucumber:ok"
    task stats: "cucumber:statsetup"

    task default: :cucumber
  rescue LoadError
    desc "Cucumber task is not available"
    task :cucumber do
      abort "Cucumber rake task is not available. Be sure to install cucumber as a gem"
    end
  end
end
