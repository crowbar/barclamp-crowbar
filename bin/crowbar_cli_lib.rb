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

require 'rubygems'
require 'net/http'
require 'net/http/digest_auth'
require 'uri'
require 'json'
require 'getoptlong'

# This class is designed at the helper for all Crowbar CLI modules
class CrowbarCliLib

  class << self; 
    attr_accessor :options, :username, :password, :hostname, :port, :url, :data, :file, :timeout
    attr_accessor :headers, :key, :allow_zero_args, :debug
  end
  
    
  @options = [
    [ [ '--help', '-h', GetoptLong::NO_ARGUMENT ], "--help or -h - help" ],
    [ [ '--username', '-U', GetoptLong::REQUIRED_ARGUMENT ], "--username <username> or -U <username>  - specifies the username" ],
    [ [ '--password', '-P', GetoptLong::REQUIRED_ARGUMENT ], "--password <password> or -P <password>  - specifies the password" ],
    [ [ '--hostname', '-n', GetoptLong::REQUIRED_ARGUMENT ], "--hostname <name or ip> or -n <name or ip>  - specifies the destination server" ],
    [ [ '--port', '-p', GetoptLong::REQUIRED_ARGUMENT ], "--port <port> or -p <port> - specifies the destination server port" ],
    [ [ '--debug', '-d', GetoptLong::NO_ARGUMENT ], "--debug or -d - turns on debugging information" ],
    [ [ '--data', GetoptLong::REQUIRED_ARGUMENT ], "--data <data> - used by create or edit as data (must be in json format)" ],
    [ [ '--file', GetoptLong::REQUIRED_ARGUMENT ], "--file <file> - used by create or edit as data when read from a file (must be in json format)" ],
    [ [ '--timeout', GetoptLong::REQUIRED_ARGUMENT ], "--timeout <seconds> - timeout in seconds for read http reads" ]
  ]
  
  @debug = false
  @hostname = ENV["CROWBAR_IP"]
  # DO NOT CHANGE THE FORMAT OF THE NEXT 2 LINES.
  # gather_cli relies on the exact spacing of them to 
  # rewrite the addreses when it is serving the CLI.
  @hostname = "127.0.0.1" unless @hostname
  @port = 3000
  @url = nil
  @headers = {
    "Accept" => "application/json",
    "Content-Type" => "application/json"
  }
  @data = ""
  @allow_zero_args = false
  @timeout = 500
  @barclamp="machines"
  @key = ENV["CROWBAR_KEY"]
  if @key
    @username=@key.split(':',2)[0]
    @password=@key.split(':',2)[1]
  end
    
  def uri_path(path=nil)
    base = (CrowbarCliLib.url || "http://#{CrowbarCliLib.hostname}:#{CrowbarCliLib.port}") + (path.nil? ? "" : "/#{path}")
    uri = URI.parse(base)
    uri.user=CrowbarCliLib.username
    uri.password=CrowbarCliLib.password
  end

  def options_parse()
    sub_options = CrowbarCliLib.options.map { |x| x[0] }
    lsub_options = CrowbarCliLib.map { |x| [ x[0][0], x[2] ] }
    opts = GetoptLong.new(*sub_options)
    opts.each do |opt, arg|
      case opt
        when '--help'
          usage 0
        when '--debug'
          CrowbarCliLib.debug = true
        when '--hostname'
          CrowbarCliLib.hostname = arg
        when '--username'
          CrowbarCliLib.username = arg
        when '--password'
          CrowbarCliLib.password = arg
        when '--url'
          CrowbarCliLib.url = arg
        when '--port'
          CrowbarCliLib.port = arg.to_i
        when '--data'
          CrowbarCliLib.data = arg
        when '--timeout'
          CrowbarCliLib.timeout = arg
        when '--file'
          CrowbarCliLib.data = File.read(arg, "r")
        else
          found = false
          lsub_options.each do |x|
            next if x[0] != opt
            eval x[1]
            found = true
          end
          usage -1 unless found
      end
    end
  
    STDERR.puts "CROWBAR_KEY not set, will not be able to authenticate!" if CrowbarCliLib.username.nil? or CrowbarCliLib.password.nil?
    STDERR.puts "Please set CROWBAR_KEY or use -U and -P" if CrowbarCliLib.username.nil? or CrowbarCliLib.password.nil?
    if ARGV.length == 0 and !CrowbarCliLib.allow_zero_args
      usage -1
    end
  end

end