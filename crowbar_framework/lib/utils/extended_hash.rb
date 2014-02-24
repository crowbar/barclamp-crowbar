# -*- encoding : utf-8 -*-
# Copyright 2011-2013, Dell
# Copyright 2013, SUSE LINUX Products GmbH
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# Author: Dell Crowbar Team
# Author: SUSE LINUX Products GmbH
#

module Utils
  class ExtendedHash < Hash
    def initialize(source_hash = nil, &blk)
      deep_update(source_hash) if source_hash
      super(&blk)
    end

    class << self
      alias [] new
    end

    def id
      self["id"] ? self["id"] : super
    end

    def default(key = nil)
      if key.is_a?(Symbol) && key?(key)
        self[key]
      else
        key ? super : super()
      end
    end

    alias_method :regular_reader, :[]
    alias_method :regular_writer, :[]=

    def [](key)
      key = convert_key(key)
      regular_reader(key)
    end

    def []=(key,value)
      key = convert_key(key)
      regular_writer(key,convert_value(value))
    end

    def initializing_reader(key)
      return self[key] if key?(key)
      self[key] = ExtendedHash.new
    end

    alias_method :regular_dup, :dup

    def dup
      ExtendedHash.new(self)
    end

    alias_method :picky_key?, :key?

    def key?(key)
      picky_key?(convert_key(key))
    end

    alias_method :regular_inspect, :inspect

    def inspect
      ret = "<#{self.class.to_s}"

      keys.sort.each do |key|
        ret << " #{key}=#{self[key].inspect}"
      end

      ret << ">"

      ret
    end

    alias_method :to_s, :inspect

    def deep_merge(other_hash)
      dup.deep_merge!(other_hash)
    end

    def deep_update(other_hash)
      other_hash = other_hash.to_hash if other_hash.is_a?(ExtendedHash)
      other_hash = other_hash.stringify_keys

      other_hash.each_pair do |k,v|
        k = convert_key(k)
        self[k] = self[k].to_extended if self[k].is_a?(Hash) unless self[k].is_a?(ExtendedHash)

        if self[k].is_a?(Hash) && other_hash[k].is_a?(Hash)
          self[k] = self[k].deep_merge(other_hash[k]).dup
        else
          self.send(k + "=", convert_value(other_hash[k],true))
        end
      end
    end

    alias_method :deep_merge!, :deep_update

    def update(other_hash)
      other_hash.each_pair do |key, value|
        if respond_to?(convert_key(key) + "=")
          self.send(convert_key(key) + "=", convert_value(value))
        else
          regular_writer(convert_key(key), convert_value(value))
        end
      end

      self
    end

    alias_method :merge!, :update

    def to_hash
      Hash.new(default).merge(self)
    end

    def method_missing(method_name, *args)
      if (match = method_name.to_s.match(/(.*)=$/)) && args.size == 1
        self[match[1]] = args.first
      elsif (match = method_name.to_s.match(/(.*)\?$/)) && args.size == 0
        key?(match[1])
      elsif (match = method_name.to_s.match(/(.*)!$/)) && args.size == 0
        initializing_reader(match[1])
      elsif key?(method_name)
        self[method_name]
      elsif match = method_name.to_s.match(/^([a-z][a-z0-9A-Z_]+)$/)
        default(method_name)
      else
        super
      end
    end

    protected

    def convert_key(key)
      key.to_s.gsub("-", "_")
    end

    def convert_value(value, dup = false)
      case value
      when Hash
        value = value.dup if value.is_a?(ExtendedHash) && dup
        value.is_a?(ExtendedHash) ? value : value.to_extended
      when Array
        value.collect{ |e| convert_value(e) }
      else
        value
      end
    end
  end
end

class Hash
  def to_extended
    mash = Utils::ExtendedHash.new(self)
    mash.default = default

    mash
  end

  def stringify_keys
    dup.stringify_keys!
  end

  def stringify_keys!
    keys.each do |k|
      v = delete(k)
      self[k.to_s] = v

      v.stringify_keys! if v.is_a?(Hash)
      v.each{|p| p.stringify_keys! if p.is_a?(Hash)} if v.is_a?(Array)
    end

    self
  end
end

