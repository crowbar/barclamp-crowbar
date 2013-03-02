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
  
  STATES = { 
              Node::ERROR => 'error', 
              Node::READY => 'ready', 
              Node::UNKNOWN => 'unknown'
           }
  STATES.default = 'unknown'
  
  def state
    (self.value_actual == MARSHAL_EMPTY ? Node::UNKNOWN : self.value_actual)
  end
  
  def state=(value)
    self.value_request = Attrib.serial_in(value)
    s = value.split[0].downcase rescue 'error'
    # this is not the right way to do this, optimize
    if STATES.value? s 
      STATES.each do |key, value|
        if value.eql? s
          self.value_actual = key
          break
        end
      end
    else
      self.value_actual = Node::ERROR
    end
    self.value_actual
  end
  
  def ready?
    self.value_actual == Node::READY
  end
  
  def state_text
    STATES[value_actual] || Attrib.serial_out(value_request) || 'unknown'
  end
  
  def actual
    state
  end

  def actual=(value)
    state = value
  end

  def request=(value)
    state = value
  end
  
  def request
    state
  end
    
    # Makes the open ended state information into a subset of items for the UI
  def status
    return 'failed'
    # if you add new states then you MUST expand the PIE chart on the nodes index page
    case self.value_actual
    when "ready"
      "ready"     #green
    when "discovered", "wait", "waiting", "user", "hold", "pending", "input"
      "pending"   #flashing yellow
    when "discovering", "reset", "delete", "reinstall", "shutdown", "reboot", "poweron", "noupdate"
      "unknown"   #grey
    when "problem", "issue", "error", "failed", "fail", "warn", "warning", "fubar", "alert", "recovering"
      "failed"    #flashing red
    when "hardware-installing", "hardware-install", "hardware-installed", "hardware-updated", "hardware-updating"
      "building"  #yellow
    else
      "unready"   #spinner
    end
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
