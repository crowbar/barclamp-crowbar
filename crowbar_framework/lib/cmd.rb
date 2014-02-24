# -*- encoding : utf-8 -*-
require 'open3'

class Cmd
  def self.run(params)
    raise ArgumentError, "Cmd.run expects array consisting of a command and its arguments." unless params.kind_of?(Array)
    stdin, stdout, stderr = Open3.popen3(*params)
    stdout.read
  end
end

