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

if AppConfig[:use_bundler]
  begin
    require "bundler"
  rescue LoadError
    raise "Could not load the bundler gem. Install it with `gem install bundler` and then `bundle install`."
  end

  if Gem::Version.new(Bundler::VERSION) <= Gem::Version.new("0.9.24")
    raise RuntimeError, "Your bundler version is too old for Rails 2.3. Run `gem install bundler` to upgrade."
  end

  begin
    ENV["BUNDLE_GEMFILE"] = File.expand_path("../../Gemfile", __FILE__)
    Bundler.setup
  rescue Bundler::GemNotFound
    raise RuntimeError, "Bundler couldn't find some gems. Did you run `bundle install`?"
  end
end
