#
# Copyright 2009, Opscode, Inc.
# Copyright 2008, Steve Midgley
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


# Copied from https://github.com/opscode/chef/blob/master/lib/chef/mixin/deep_merge.rb
# The hash_only_merge API is only available starting with Chef 11.
module HashOnlyMerge
  def self.hash_only_merge(merge_onto, merge_with)
    hash_only_merge!(merge_onto.dup, merge_with.dup)
  end

  # Deep merge without Array merge.
  # `merge_onto` is the object that will "lose" in case of conflict.
  # `merge_with` is the object whose values will replace `merge_onto`s
  # values when there is a conflict.
  def self.hash_only_merge!(merge_onto, merge_with)
    # If there are two Hashes, recursively merge.
    if merge_onto.kind_of?(Hash) && merge_with.kind_of?(Hash)
      merge_with.each do |key, merge_with_value|
        merge_onto[key] = hash_only_merge!(merge_onto[key], merge_with_value)
      end
      merge_onto

      # If merge_with is nil, don't replace merge_onto
    elsif merge_with.nil?
      merge_onto

      # In all other cases, replace merge_onto with merge_with
    else
      merge_with
    end
  end
end
