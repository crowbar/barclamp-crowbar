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

if ENV["ENABLE_PROFILER"]
  require "rack-mini-profiler"
  Rack::MiniProfilerRails.initialize! Rails.application

  begin
    ::Rack::MiniProfiler.profile_method Chef::Search::Query, :search do |model, query|
      "Chef search: #{model} #{query || "all"}"
    end
  rescue
    Rails.logger.warn "Failed to initialize profiler for chef calls"
  end
end
