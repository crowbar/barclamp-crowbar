#
# Copyright 2015, SUSE LINUX Products GmbH
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


# With deferred transaction mode, used by ActiveRecord by default, we were
# getting SQLite3::BusyException when two processess attempted to do an update
# to IPMI proposal at the same time. Using immediate transaction mode
# (https://www.sqlite.org/lang_transaction.html) prevents two threads/processess
# to begin an (immediate) transaction simultaneously and seems to alleviate
# the issue.

module ActiveRecord
  module ConnectionAdapters
    class SQLite3Adapter < AbstractAdapter
      def begin_db_transaction
        log('begin transaction',nil) { @connection.transaction(:immediate) }
      end
    end
  end
end
