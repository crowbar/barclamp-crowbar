#!/usr/bin/ruby
# Copyright 2011, Dell
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
# Author: RobHirschfeld
#

  # the 1st choice is to use the code from the framework since it is most up to date
  # however, that code is not always available when installing
  fw_lib = File.join '/opt', 'dell', 'crowbar_framework', 'lib', 'barclamp_lib'
  if File.exists? fw_lib+'.rb'
    require fw_lib
  else
    require File.join '/opt', 'dell', 'barclamps', 'crowbar', 'crowbar_framework', 'lib', 'barclamp_lib'
  end

  debug = DEBUG

  # this is used by the install-chef installer script 
  if __FILE__ == $0
    path = (ARGV[0].nil? ? File.join('/opt','dell','barclamps') : ARGV[0])
    puts "Using #{path}" if debug
    barclamps = {}
    Dir.entries(path).each do |dir|
      if File.directory? File.join(path,dir) 
        bc_file = File.join path, dir, 'crowbar.yml'
        if File.exist? bc_file
          barclamp = YAML.load_file bc_file
          name = (!barclamp["barclamp"].nil? and !barclamp["barclamp"]["name"].nil? ? barclamp["barclamp"]["name"] : nil)
          unless name.nil?
            order = (!barclamp["crowbar"].nil? and !barclamp["crowbar"]["order"].nil? ? barclamp["crowbar"]["order"].to_i : 1000)
            barclamps[name] = {:dir=>dir, :name=>name, :file=>bc_file, :order=>order}
          else
            puts "Did not track data related to #{bc_file} because barclamp/name not found" if debug
          end
        end
      end
    end
    barclamps = barclamps.sort_by { |k ,v| v[:order] }
    barclamps.each do |name, bc|      
      begin
        barclamp = YAML.load_file bc[:file]
        bc_install bc[:name], File.join(path, bc[:dir]), barclamp
        puts "#{bc[:order]}: installed #{path}/#{bc[:dir]}/#{bc[:name]}" if debug
      rescue
        exit -3    
      end
    end
    exit 0 
  end
  