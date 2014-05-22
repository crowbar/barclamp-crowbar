# -*- encoding : utf-8 -*-
# Copyright 2012, Dell Inc., Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

default[:crowbar][:simple_proposal_ui] = true

case node["platform"]
when "suse"
  default["crowbar"]["recipes"] = %w(

  )

  default["crowbar"]["gems"] = %w(

  )

  default["crowbar"]["packages"] = %w(
    curl
    sqlite3
    sqlite3-devel
    rubygem-activerecord-session_store
    rubygem-activeresource
    rubygem-bcrypt
    rubygem-bootstrap-sass
    rubygem-cells
    rubygem-closure-compiler
    rubygem-cocaine
    rubygem-delayed_job_active_record
    rubygem-dotenv-rails
    rubygem-font-awesome-rails
    rubygem-gli
    rubygem-haml-rails
    rubygem-hashie
    rubygem-js-routes
    rubygem-json
    rubygem-kwalify
    rubygem-nokogiri
    rubygem-pry-rails
    rubygem-rack
    rubygem-rails
    rubygem-rainbows-rails
    rubygem-redcarpet
    rubygem-sass-rails
    rubygem-simple-navigation
    rubygem-simple_navigation_renderers
    rubygem-sqlite3
    rubygem-net-http-digest_auth
    rubygem-ruby-shadow
  )
when "debian", "ubuntu"
  default["crowbar"]["recipes"] = %w(
    bluepill
  )

  default["crowbar"]["gems"] = %w(
    activerecord-session_store-0.1.0
    activeresource-4.0.0
    bcrypt-3.1.7
    bootstrap-sass-3.1.1.1
    cells-3.10.0
    closure-compiler-1.1.10
    cocaine-0.5.4
    delayed_job_active_record-4.0.1
    dotenv-rails-0.10.0
    font-awesome-rails-4.0.3.1
    gli-2.9.0
    haml-rails-0.5.3
    hashie-2.0.5
    js-routes-0.9.7
    json-1.8.1
    kwalify-0.7.2
    nokogiri-1.6.1
    pry-rails-0.3.2
    rack-1.5.2
    rails-4.1.0
    rainbows-rails-1.0.1
    redcarpet-3.1.1
    sass-rails-4.0.3
    simple-navigation-3.12.2
    simple_navigation_renderers-1.0.2
    sqlite3-1.3.9
    net-http-digest_auth-1.4
    ruby-shadow-2.3.3
  )

  default["crowbar"]["packages"] = %w(
    curl
    sqlite
    libsqlite3-dev
  )
when "redhat", "centos"
  default["crowbar"]["recipes"] = %w(
    bluepill
  )

  default["crowbar"]["gems"] = %w(
    activerecord-session_store-0.1.0
    activeresource-4.0.0
    bcrypt-3.1.7
    bootstrap-sass-3.1.1.1
    cells-3.10.0
    closure-compiler-1.1.10
    cocaine-0.5.4
    delayed_job_active_record-4.0.1
    dotenv-rails-0.10.0
    font-awesome-rails-4.0.3.1
    gli-2.9.0
    haml-rails-0.5.3
    hashie-2.0.5
    js-routes-0.9.7
    json-1.8.1
    kwalify-0.7.2
    nokogiri-1.6.1
    pry-rails-0.3.2
    rack-1.5.2
    rails-4.1.0
    rainbows-rails-1.0.1
    redcarpet-3.1.1
    sass-rails-4.0.3
    simple-navigation-3.12.2
    simple_navigation_renderers-1.0.2
    sqlite3-1.3.9
    net-http-digest_auth-1.4
    ruby-shadow-2.3.3
  )

  default["crowbar"]["packages"] = %w(
    curl
    sqlite
    sqlite-devel
  )
else
  raise "Platform #{node["platform"]} is not supported"
end
