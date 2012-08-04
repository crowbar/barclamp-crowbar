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

class Node < ActiveRecord::Base
  attr_accessible :name, :description
  
  def method_missing(m,*args,&block)
    puts "Node #{name} #{args.inspect} #{block.inspect}"
    #unless node.respond_to?(m)
    #  Rails.logger.fatal("Cannot delegate method #{m} to #{@node.class}")
    #else
    #  case
    #  when args && block_given? then @node.send(m,*args,&block)
    #  when block_given? then @node.send(m,&block)
    #  when args then @node.send(m,*args)
    #  else @node.send(m)
    #  end
    #end
  end

end
