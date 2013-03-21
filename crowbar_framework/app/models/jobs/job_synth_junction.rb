# Copyright 2013, Dell
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
# Author: aabes

module Jobs

  #
  # This class is a synthetic job - it's main purpose in life is to aggregate
  # upstream dependencies into just 1 dependency. This helps considerably simplify the graph
  # changing the dependency count from O(m x n) to O(m+n) where m/n are depentdent and depencency

  class JobSyncJunction < Jobs::DependentJob

    # the perform method is only called when all the dependencies are ready... and by definition
    # when the dependencies are ready, so is this job.
    def perform
      set_done(true)
      save!
    end
  end

end
