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

SimpleNavigation::Configuration.run do |navigation|       
  menu = Nav.item('root').first
  navigation.items do |primary|
    menu.children.sort_by{|n| n.order}.each do |item| # Top Nav
      if item.item != 'root' and item.path =~ /(.*)_path/
        begin   
          name = t(item.name)
          title = t(item.description, :default=>t(item.name))
          if item.development
            name = "[#{name}]"
            title = "Dev Mode: "+title
          end
          primary.item item.item.to_sym, name, eval(item.path) do |secondary|
            # TODO ZEHICLE - put help listing back!
            #if item.item.eql? 'help'
             # Doc.find_by_name('root').children.each do |doc|
             #   secondary.item doc.name.to_sym, t(doc.name, :scope=>'nav.books'), docs_path(:id=>doc.name.html_safe), {:title=>doc.description }
             # end
            #end
            if item.item.eql? 'scaffold'
              build_scaffold_nav(secondary, item)
            else
            # render menu items from the database
            item.children.each do |nav|
              if nav.path.starts_with? 'http'
                secondary.item nav.item.to_sym, t(nav.name), nav.path.to_s, {:title=>t(nav.description, :default=>t(nav.name)), :link => { :target => "_blank" } } 
              elsif nav.path =~ /(.*)_path/ 
                if nav.development
                  secondary.item nav.item.to_sym, "[#{t(nav.name)}]", eval(nav.path), {:title=>"Dev Mode: #{t(nav.description, :default=>t(nav.name))}"} 
                else
                    secondary.item nav.item.to_sym, t(nav.name), eval(nav.path), {:title=>t(nav.description, :default=>t(nav.name))}  do |tertiary|
                      # deployments helper
                    begin
                      if nav.name.eql? 'nav.deployments'
                        Deployment.all.each do |d|
                          tertiary.item nav.item.to_sym, d.name, deployment_head_path(d.id), {:title=>d.description }
                        end
                      end            
                    rescue
                      # each the exception in the chidlren for now
                    end
                  end
                end
              end 
            end
            end
          end
        rescue Exception => e
          primary.item :menu_error, "#{t 'nav.error'}: #{item.item}", ''
          puts "render error #{e.inspect}" if Rails.env.development?
        end
      end
    end
  end
end


