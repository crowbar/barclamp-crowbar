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
      if item.item != 'root' and item.path =~ /(.*)_path/ 
        primary.item item.item.to_sym, t(item.name), eval(item.path), {:title=>t(item.description, :default=>t(item.name))} do |secondary|
          item.children.each do |nav|
            if nav.path.starts_with? 'http'
              secondary.item nav.item.to_sym, t(nav.name), nav.path.to_s, {:title=>t(nav.description, :default=>t(nav.name)), :link => { :target => "_blank" } } 
            elsif nav.path =~ /(.*)_path/ 
              secondary.item nav.item.to_sym, t(nav.name), eval(nav.path), {:title=>t(nav.description, :default=>t(nav.name))} 
            end 
          end
        end  
      end
    end
  end
end


