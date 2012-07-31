# Copyright 2012, Dell
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

class Jobs::BaseJob

  def enqueue(job)
    record_stat 'enqueue'
  end

  def perform
    record_stat 'perform'
  end

  def before(job)
    record_stat 'start'
  end

  def after(job)
    record_stat 'after'
  end

  def success(job)
    record_stat 'success'
  end

  def error(job, exception)
    record_stat 'error'
  end

  def failure
    record_stat 'failure'
  end

private

  def record_stat(item)
    str = "#{self.class.name.demodulize}/#{item}"
    EventQueue.publish(Events::JobEvent.new(str))
  end

end


