require 'socket'
module RemoteNode
  # hosts for allowing run ssh commands
  # @param host [string] hostname or IP
  # @param timeout [integer] timeout in seconds
  def self.ready? host, timeout=60
    sleep(10)
    nobj = NodeObject.find_node_by_name(host)
    start = Time.now.to_i
    while (Time.now.to_i - start) < timeout do
      begin
        TCPSocket.new(host, 22)
        if %w(redhat centos).include?(nobj[:platform])
          raise "next" unless system("sudo -i -u root -- ssh root@#{host} 'runlevel | grep \"N 3\"'")
        else
          raise "next" unless system("sudo -i -u root -- ssh root@#{host} 'last | head -n1 | grep -vq down'")
        end
        return true
      rescue
        sleep(10)
        next
      end
    end
    return false
  end
end

