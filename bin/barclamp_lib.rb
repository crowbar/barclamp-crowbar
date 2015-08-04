#
# Copyright 2011-2013, Dell
# Copyright 2013-2014, SUSE LINUX Products GmbH
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
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

@debug = false
@hostname = ENV["CROWBAR_IP"]
@hostname = "127.0.0.1" unless @hostname
@port = ENV["CROWBAR_PORT"] || 3000
@headers = {
  "Accept" => "application/json",
  "Content-Type" => "application/json"
}
@data = ""
@allow_zero_args = false
@timeout = 500
@crowbar_key_file = '/etc/crowbar.install.key'

#
# Parsing options can be added by adding to this list before calling opt_parse
#
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

#
# New commands can be added by adding to this list before calling run_command
#
# Proposal is an example of running sub-commands
#
@proposal_commands = {
  "list" => [ "proposal_list", "list - show a list of current proposals" ],
  "create" => [ "proposal_create ARGV.shift", "create <name> - create a proposal" ],
  "show" => [ "proposal_show ARGV.shift", "show <name> - show a specific proposal" ],
  "edit" => [ "proposal_edit ARGV.shift", "edit <name> - edit a new proposal" ],
  "delete" => [ "proposal_delete ARGV.shift", "delete <name> - delete a proposal" ],
  "commit" => [ "proposal_commit ARGV.shift", "commit <name> - Commit a proposal to active" ],
  "dequeue" => [ "proposal_dequeue ARGV.shift", "dequeue <name> - Dequeue a proposal to active" ]
}

@commands = {
  "help" => [ "help", "help - this page" ],
  "api_help" => [ "api_help", "crowbar API help - help for this barclamp." ],
  "list" => [ "list", "list - show a list of current configs" ],
  "show" => [ "show ARGV.shift", "show <name> - show a specific config" ],
  "delete" => [ "delete ARGV.shift", "delete <name> - delete a config" ],
  "proposal" => [ "run_sub_command(@proposal_commands, ARGV.shift)", "proposal - Proposal sub-commands", @proposal_commands ],
  "elements" => [ "elements", "elements - List elements of a #{@barclamp} deploy" ],
  "element_node" => [ "element_node ARGV.shift", "element_node <name> - List nodes that could be that element" ],
  "transition" => [ "transition(ARGV.shift,ARGV.shift)", "transition <name> <state> - Transition machine named name to state" ]
}


def print_commands(cmds, spacer = "  ")
  cmds.each do |key, command|
    puts "#{spacer}#{command[1]}"
    print_commands(command[2], "  #{spacer}") if command[0] =~ /run_sub_command\(/
  end
end

def usage (rc)
  puts "Usage: crowbar #{@barclamp} [options] <subcommands>"
  @options.each do |options|
    puts "  #{options[1]}"
  end
  print_commands(@commands.sort)
  exit rc
end

def help
  usage 0
end

def debug(msg)
  puts msg if @debug
end

def authenticate(req, uri, data = nil)
  uri.user = @username
  uri.password = @password

  h = Net::HTTP.new uri.host, uri.port
  h.read_timeout = @timeout

  r = req.new uri.request_uri, @headers
  r.body = data if data

  res = h.request r

  debug "(r) hostname: #{uri.host}:#{uri.port}"
  debug "(r) request: #{uri.path}"
  debug "(r) method: #{req::METHOD}"
  debug "(r) return code: #{res.code}"
  debug "(r) return body: #{res.body}"
  res.each_header do |h, v|
    debug "(r) return #{h}: #{v}"
  end

  if res["www-authenticate"]
    digest = Net::HTTP::DigestAuth.new
    auth = digest.auth_header uri, res["www-authenticate"], req::METHOD

    r = req.new uri.request_uri, @headers
    r.body = data if data
    r.add_field "Authorization", auth

    res = h.request r

    debug "(a) hostname: #{uri.host}:#{uri.port}"
    debug "(a) request: #{uri.path}"
    debug "(a) method: #{req::METHOD}"
    debug "(a) return code: #{res.code}"
    debug "(a) return body: #{res.body}"
    res.each_header do |h, v|
      debug "(a) return #{h}: #{v}"
    end
  end

  res
end

def get_json(path)
  uri = URI.parse("http://#{@hostname}:#{@port}/crowbar/#{@barclamp}/1.0#{path}")
  res = authenticate(Net::HTTP::Get,uri)

  debug "(g) hostname: #{uri.host}:#{uri.port}"
  debug "(g) request: #{uri.path}"
  debug "(g) return code: #{res.code}"
  debug "(g) return body: #{res.body}"

  return [res.body, res.code.to_i ] if res.code.to_i != 200

  struct = JSON.parse(res.body)

  debug "(g) JSON parse structure = #{struct.inspect}"

  return [struct, 200]
end

def post_json(path, data)
  uri = URI.parse("http://#{@hostname}:#{@port}/crowbar/#{@barclamp}/1.0#{path}")
  res = authenticate(Net::HTTP::Post,uri,data)

  debug "(post) hostname: #{uri.host}:#{uri.port}"
  debug "(post) request: #{uri.path}"
  debug "(post) data: #{@data}"
  debug "(post) return code: #{res.code}"
  debug "(post) return body: #{res.body}"

  [res.body, res.code.to_i ]
end

def put_json(path, data)
  uri = URI.parse("http://#{@hostname}:#{@port}/crowbar/#{@barclamp}/1.0#{path}")
  res = authenticate(Net::HTTP::Put,uri,data)

  debug "(put) hostname: #{uri.host}:#{uri.port}"
  debug "(put) request: #{uri.path}"
  debug "(put) data: #{@data}"
  debug "(put) return code: #{res.code}"
  debug "(put) return body: #{res.body}"

  [res.body, res.code.to_i ]
end

def delete_json(path)
  uri = URI.parse("http://#{@hostname}:#{@port}/crowbar/#{@barclamp}/1.0#{path}")
  res = authenticate(Net::HTTP::Delete,uri)

  debug "(d) hostname: #{uri.host}:#{uri.port}"
  debug "(d) request: #{uri.path}"
  debug "(d) return code: #{res.code}"
  debug "(d) return body: #{res.body}"

  [res.body, res.code.to_i ]
end


def list
  struct = get_json("/")

  if struct[1] != 200
    [ "Failed to talk to service list: #{struct[1]}: #{struct[0]}", 1 ]
  elsif struct[0].nil? or struct[0].empty?
    [ "No current configurations", 0 ]
  else
    out = ""
    struct[0].sort.each do |name|
      out = out + "\n" if out != ""
      out = out + "#{name}"
    end
    [ out, 0 ]
  end
end

def api_help
  struct=get_json("/help")
  if struct[1] != 200
    [ "Failed to talk to service list: #{struct[1]}: #{struct[0]}", 1 ]
  elsif struct[0].nil? or struct[0].empty?
    [ "No help", 0 ]
  else
    [ jj(struct[0]), 0 ]
  end
end

def show(name)
  usage(-1) if name.nil? or name == ""

  struct = get_json("/#{name}")

  if struct[1] == 200
    [ "#{JSON.pretty_generate(struct[0])}", 0 ]
  elsif struct[1] == 404
    [ "No current configuration for #{name}", 1 ]
  else
    [ "Failed to talk to service show: #{struct[1]}: #{struct[0]}", 1 ]
  end
end

def delete(name)
  usage(-1) if name.nil? or name == ""

  struct = delete_json("/#{name}")

  if struct[1] == 200
    [ "Deleted #{name}", 0 ]
  elsif struct[1] == 404
    [ "Delete failed for #{name}: Not Found", 1 ]
  else
    [ "Failed to talk to service delete: #{struct[1]}: #{struct[0]}", 1 ]
  end
end

def proposal_list
  struct = get_json("/proposals/")

  if struct[1] != 200
    [ "Failed to talk to service proposal list: #{struct[1]}: #{struct[0]}", 1 ]
  elsif struct[0].nil? or struct[0].empty?
    [ "No current proposals", 0 ]
  else
    out = ""
    struct[0].sort.each do |name|
      out = out + "\n" if out != ""
      out = out + "#{name}"
    end
    [ out, 0 ]
  end
end

def proposal_show(name)
  usage(-1) if name.nil? or name == ""

  struct = get_json("/proposals/#{name}")

  if struct[1] == 200
    [ "#{JSON.pretty_generate(struct[0])}", 0 ]
  elsif struct[1] == 404
    [ "No current proposal for #{name}", 1 ]
  else
    [ "Failed to talk to service proposal show: #{struct[1]}: #{struct[0]}", 1 ]
  end
end

def proposal_create(name)
  usage(-1) if name.nil? or name == ""

  @data = "{\"id\":\"#{name}\"}" if @data.nil? or @data == ""

  struct = put_json("/proposals", @data)

  if struct[1] == 200
    [ "Created #{name}", 0 ]
  else
    [ "Failed to talk to service proposal create: #{struct[1]}: #{struct[0]}", 1]
  end
end

def proposal_edit(name)
  usage(-1) if name.nil? or name == ""

  if @data.nil? or @data == ""
    struct = get_json("/proposals/#{name}")

    if struct[1] == 200
      require 'tempfile'

      file = Tempfile.new("proposal-#{name}")
      begin
        file.write(JSON.pretty_generate(struct[0]))
      ensure
        file.close
      end

      editor = ENV['EDITOR'] or "/usr/bin/vi"
      system("#{editor} #{file.path}")

      begin
        file.open
        #@data = JSON.pretty_generate(file.read)
        @data = JSON.pretty_generate(JSON.parse(file.read))
      ensure
        file.close
        file.unlink
      end
    elsif struct[1] == 404
      [ "No current proposal for #{name}", 1 ]
    else
      [ "Failed to talk to service proposal show: #{struct[1]}: #{struct[0]}", 1 ]
    end
  end

  struct = post_json("/proposals/#{name}", @data)

  if struct[1] == 200
    [ "Edited #{name}", 0 ]
  elsif struct[1] == 404
    [ "Failed to edit: #{name} : Not Found", 1 ]
  elsif struct[1] == 400
    [ "Failed to edit: #{name} : Errors in data\n#{struct[0]}", 1 ]
  else
    [ "Failed to talk to service proposal edit: #{struct[1]}: #{struct[0]}", 1 ]
  end
end

def proposal_delete(name)
  usage(-1) if name.nil? or name == ""

  struct = delete_json("/proposals/#{name}")

  if struct[1] == 200
    [ "Deleted #{name}", 0 ]
  elsif struct[1] == 404
    [ "Delete failed for #{name}: Not Found", 1 ]
  else
    [ "Failed to talk to service delete: #{struct[1]}: #{struct[0]}", 1 ]
  end
end

def proposal_commit(name)
  usage(-1) if name.nil? or name == ""

  struct = post_json("/proposals/commit/#{name}", @data)

  if struct[1] == 200
    [ "Committed #{name}", 0 ]
  elsif struct[1] == 202
    [ "Queued #{name} because #{struct[0]}", 0 ]
  else
    [ "Failed to talk to service proposal commit: #{struct[1]}: #{struct[0]}", 1 ]
  end
end

def proposal_dequeue(name)
  usage(-1) if name.nil? or name == ""

  struct = delete_json("/proposals/dequeue/#{name}")

  if struct[1] == 200
    [ "Dequeued #{name}", 0 ]
  else
    [ "Failed to talk to service proposal dequeue: #{struct[1]}: #{struct[0]}", 1 ]
  end
end

def elements
  struct = get_json("/elements")

  if struct[1] != 200
    [ "Failed to talk to service elements: #{struct[1]}: #{struct[0]}", 1 ]
  elsif struct[0].nil? or struct[0].empty?
    [ "No current elements", 1 ]
  else
    out = ""
    struct[0].each do |name|
      out = out + "\n" if out != ""
      out = out + "#{name}"
    end
    [ out, 0 ]
  end
end

def element_node(element)
  usage(-1) if element.nil? or element == ""

  struct = get_json("/elements/#{element}")

  if struct[1] != 200
    [ "Failed to talk to service element_node: #{struct[1]}: #{struct[0]}", 1 ]
  elsif struct[0].nil? or struct[0].empty?
    [ "No nodes for #{element}", 1 ]
  else
    out = ""
    struct[0].sort.each do |name|
      out = out + "\n" if out != ""
      out = out + "#{name}"
    end
    [ out, 0 ]
  end
end

def transition(name, state)
  usage(-1) if name.nil? or name == ""

  data = {
    "name" => name,
    "state" => state
  }
  struct = post_json("/transition/default", data.to_json)

  if struct[1] == 200
    [ "Transitioned #{name}", 0 ]
  else
    [ "Failed to talk to service transition: #{struct[1]}: #{struct[0]}", 1 ]
  end
end



### Start MAIN ###

def opt_parse
  get_user_password

  standard_opt_parse

  if ARGV.length == 0 and !@allow_zero_args
    usage -1
  end

  check_user_password
end

def get_user_password
  key = ENV["CROWBAR_KEY"]
  if key.nil? and ::File.exists?(@crowbar_key_file) and ::File.readable?(@crowbar_key_file)
    begin
      key = File.read(@crowbar_key_file).strip
    rescue => e
      warn "Unable to read crowbar key from #{@crowbar_key_file}: #{e}"
    end
  end

  if key
    @username, @password = key.split(":",2)
  end
end

def standard_opt_parse
  sub_options = @options.map { |x| x[0] }
  opts = GetoptLong.new(*sub_options)

  opts.each do |opt, arg|
    if ! parse_standard_opt(opt, arg)
      parse_extra_opt(opt, arg)
    end
  end
end

def parse_standard_opt(opt, arg)
  case opt
  when '--help'
    usage 0
  when '--debug'
    @debug = true
  when '--hostname'
    @hostname = arg
  when '--username'
    @username = arg
  when '--password'
    @password = arg
  when '--port'
    @port = arg.to_i
  when '--data'
    @data = arg
  when '--timeout'
    @timeout = arg.to_i
  when '--file'
    @data = File.read(arg)
  else
    return false
  end

  return true
end

def parse_extra_opt(opt, arg)
  found = false
  @options.each do |x|
    next unless x[0].include? opt
    x[2].call(opt, arg)
    found = true
  end
  usage(-1) unless found
end

def check_user_password
  if @username.nil? or @password.nil?
    STDERR.puts "CROWBAR_KEY not set, will not be able to authenticate!"
    STDERR.puts "Please set CROWBAR_KEY or use -U and -P"
    exit 1
  end
end

def run_sub_command(cmds, subcmd)
  cmd = cmds[subcmd]
  usage(-2) if cmd.nil?
  eval cmd[0]
end

def run_command()
  run_sub_command(@commands, ARGV.shift)
end

def main()
  opt_parse
  res = run_command
  puts res[0]
  exit res[1]
end
