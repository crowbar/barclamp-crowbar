# Copyright 2013, Dell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
#
# This model is a stub for the Jig override system
# It is NOT installed by default, but can be used for testing or as a model

require 'json'

class BarclampCrowbar::Jig < Jig

  def execute(cycle)
    Rails.logger.info("ScriptJig Cycle #{cycle.name}")
    # retrieve the next turn for jig
    # get all node-roles associated w/ turn
    queue = cycle.queue(self)
    queue.each do |nr|
      # get role from nr
      # tmpdir=%x{ssh $node mktemp -d /tmp/scriptjig-XXXXX}
      # scp \barclamp\script\role\*.sh $node:#{tmpdir}
      # scripot.sort.each do |s|
      #   res = %X{ssh $node #{tmpdir}/#{s}} $outbound
      #   raise "O noes!" if $? != 0
      # end
      # ssh $node rm -rf #{tmpdir}
      # run script(s) n directory  [node-role data merge]
      # ssh to node
      # run script from meta-data
      # 
    end

  end

  def create_node(node)
    Rails.logger.info("ScriptJig Creating node: #{node.name}")
    # ? generate a SSH pub/private key pair
    # ? put in node: /user/root authorized_keys file
    # return JSON to be returned to the node
    {}
  end

  def delete_node(node)
    Rails.logger.info("ScriptJig Deleting node: #{node.name}")    
  end

end


