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

namespace :crowbar do
  desc "Run migration on proposals"
  task :migrate, [:barclamps] => :environment do |t, args|
    args.with_defaults(barclamps: "all")
    barclamps = args[:barclamps].split(" ")

    require "schema_migration"

    if barclamps.include?("all")
      SchemaMigration.run
    else
      barclamps.each do |barclamp|
        SchemaMigration.run_for_bc barclamp
      end
    end
  end

  [
    :production,
    :development
  ].each do |env|
    namespace env do
      desc "Run migration on proposals for #{env} environment"
      task :migrate, [:barclamps] do |t, args|
        RAILS_ENV = env.to_s
        Rake::Task["crowbar:migrate"].invoke(args[:barclamps])
      end
    end
  end
end
