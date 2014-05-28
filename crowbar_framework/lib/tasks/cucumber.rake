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

unless ARGV.any? {|a| a =~ /^gems/}
  vendored_cucumber_bin = Dir["#{Rails.root}/vendor/{gems,plugins}/cucumber*/bin/cucumber"].first
  $LOAD_PATH.unshift(File.dirname(vendored_cucumber_bin) + "/../lib") unless vendored_cucumber_bin.nil?

  begin
    require "cucumber/rake/task"

    namespace :cucumber do
      Cucumber::Rake::Task.new({:ok => "db:test:prepare"}, "Run features that should pass") do |t|
        t.binary = vendored_cucumber_bin
        t.fork = true
        t.profile = "default"
      end

      Cucumber::Rake::Task.new({:wip => "db:test:prepare"}, "Run features that are being worked on") do |t|
        t.binary = vendored_cucumber_bin
        t.fork = true
        t.profile = "wip"
      end

      Cucumber::Rake::Task.new({:rerun => "db:test:prepare"}, "Record failing features and run only them if any exist") do |t|
        t.binary = vendored_cucumber_bin
        t.fork = true
        t.profile = "rerun"
      end

      desc "Run all features"
      task :all => [:ok, :wip]
    end

    desc "Alias for cucumber:ok"
    task :cucumber => "cucumber:ok"

    task :default => :cucumber

    task :features => :cucumber do
      STDERR.puts "*** The 'features' task is deprecated. See rake -T cucumber ***"
    end
  rescue LoadError
    desc "cucumber rake task not available (cucumber not installed)"
    task :cucumber do
      abort "Cucumber rake task is not available. Be sure to install cucumber as a gem or plugin"
    end
  end
end
