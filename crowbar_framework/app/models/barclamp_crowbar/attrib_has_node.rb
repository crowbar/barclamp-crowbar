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

class BarclampCrowbar::AttribHasNode < Attrib

  before_save :special_attrib
  
  attr_protected :attrib_id

  HASNODE_NAME = 'status'
  
  STATUS = { 
              Node::ERROR => 'error', 
              Node::UNKNOWN => 'unknown',
              Node::READY => 'ready', 
              Node::UNREADY => 'unready',
              Node::PENDING => 'pending',
              Node::BUILDING => 'building'
           }
  
  def state
    (self.id_actual<0 ? Node::UNKNOWN : self.id_actual)
  end
  
  def state=(value)
    self.jig_run_id = 0    
    if value.is_a? Integer
      self.id_actual = value
      self.actual = STATUS[value]
    else
      self.actual = value
      s = value.split[0].downcase rescue 'error'
      self.id_actual = case s
      when "ready"
        Node::READY     #green
      when "discovered", "wait", "waiting", "user", "hold", "pending", "input"
        Node::PENDING   #flashing yellow
      when "discovering", "reset", "delete", "reinstall", "shutdown", "reboot", "poweron", "noupdate"
        Node::UNKNOWN   #grey
      when "problem", "issue", "error", "failed", "fail", "warn", "warning", "fubar", "alert", "recovering"
        Node::ERROR    #flashing red
      when "hardware-installing", "hardware-install", "hardware-installed", "hardware-updated", "hardware-updating"
        Node::BUILDING  #yellow
      else
        Node::UNREADY   #spinner
      end
    end
  end
  
  def ready?
    self.state == Node::READY
  end

  def status
    STATUS[state] || STATUS[Node::UNKNOWN]
  end
  
  def state_text
    self.value_actual
  end
    
  # used by the API when values are set outside of Jig runs
  def actual=(value)
    self.value_actual = value
  end
  
  def actual
    self.value_actual
  end
  
  private
  
  # this is a special purpose attrib and can only be this type
  def special_attrib
    # we're going to ENFORCE that this model uses the has_role type
    a = AttribType.find_by_id self.attrib_type_id
    if a.nil? or !a.name.eql? HASNODE_NAME
      has_node = AttribType.add :name=>HASNODE_NAME, :description=>I18n.t('model.attribs.role.has_node'), :order=>999999
      self.attrib_type_id = has_node.id
    end
  end
      
end
