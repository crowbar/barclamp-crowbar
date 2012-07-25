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
SimpleNavigation::Configuration.run do |navigation|  
  menu = Nav.find_by_item 'root'
  navigation.items do |primary|
    menu.children.each do |item|
      unless item.item == 'root'
        primary.item item.item.to_sym, t(item.name), eval(item.path), {:title=>t(item.description, :default=>t(item.name))} do |secondary|
          if item.children
            item.children.each do |subitem|
              if subitem.path.starts_with? 'http'
                secondary.item subitem.item.to_sym, t(subitem.name), subitem.path.to_s, {:title=>t(subitem.description, :default=>t(subitem.name)), :link => { :target => "_blank" } } 
              else
                secondary.item subitem.item.to_sym, t(subitem.name), eval(subitem.path), {:title=>t(subitem.description, :default=>t(subitem.name)) }
              end
            end
          end
        end  
      end
    end
  end
end
