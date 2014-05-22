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

require "sinatra/base"

class OfflineChef < Sinatra::Base
  @@cache = {}

  before do
    content_type :json
  end

  get "/search/*" do |type|
    render_search_json(type, params[:q])
  end

  get "/data/*" do |bag|
    render_json(:data_bag_item, bag.gsub("/", "-"))
  end

  get "/nodes/:fqdn" do |fqdn|
    render_json(:node, fqdn)
  end

  get "/roles/:name" do |name|
    render_json(:role, name)
  end

  post "/data/*" do |data|
    empty_json
  end

  post "/roles/*" do |role|
    empty_json
  end

  put "/roles/*" do |role|
    empty_json
  end

  post "/nodes/*" do |node|
    empty_json
  end

  put "/nodes/*" do |node|
    empty_json
  end

  private

  def logger
    Rails.logger
  end

  def empty_json
    "{}"
  end

  def render_search_json(type, query)
    type = "data_bag_item_#{type}" if search_data_bags?(type)
    cache_key = "#{type} #{query}"

    logger.debug "[OFFLINE_CHEF] searching for #{cache_key} among fixtures matching #{type}*.json"

    @@cache.fetch(cache_key) do
      jsons = read_fixtures(type)
      matches = search(type, query, jsons)
      logger.debug "[OFFLINE_CHEF] matching fixtures/results: #{jsons.count}/#{matches.count}"

      @@cache[cache_key] = {
        :rows  => matches,
        :start => 0,
        :total => matches.count
      }.to_json
    end
  end

  def render_json(type, name)
    if ["nonexistent", "missing", "remove"].any? { |key| name =~ /#{key}/ }
      status 404
      empty_json
    else
      status 200
      json_fixture(type, name)
    end
  end

  def json_fixture(type, name)
    fixture = Rails.root.join("spec", "fixtures", "offline_chef", "#{type}_#{name}.json")
    File.read(fixture)
  rescue Errno::ENOENT
    warn "#{request.request_method} #{request.url} is missing a #{fixture}"
    status 404
    empty_json
  end

  def search_data_bags?(type)
    not ["node", "role"].include?(type)
  end

  def read_fixtures(type)
    fixtures = Dir.glob(Rails.root.join("spec", "fixtures", "offline_chef", "#{type}*.json"))
    fixtures.map do |f|
      JSON.parse(File.read(f), :create_additions => false)
    end
  end

  def parse_query(query)
    query.split(/[\s\t]+/).first.split(/:/)
  end

  def matches?(key, val, data)
    (key == "*" && val == "*") || (data[key] && File.fnmatch(val, data[key].to_s))
  end

  def flatten(hash)
    {}.tap do |o|
      hash.each do |k, v|
        v.is_a?(Hash) ? o.merge!(flatten(v)) : o[k] = v
      end
    end
  end

  def search(type, query, data)
    key, val = parse_query(query)

    data.select do |d|
      matches?(key, val, flatten(d))
    end
  end
end
