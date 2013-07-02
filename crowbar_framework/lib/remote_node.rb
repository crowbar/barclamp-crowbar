require 'socket'
module RemoteNode
  # hosts for allowing run ssh commands
  # @param host [string] hostname or IP
  # @param timeout [integer] timeout in seconds
  def self.ready? host, timeout=1
    start = Time.now.to_i
    while (Time.now.to_i - start) < timeout do
      begin
        TCPSocket.new(host, 22)
        raise "next" unless system("sudo -i -u root -- ssh root@#{host} 'last | head -n1 | grep -vq down'")
        return true
      rescue
        sleep(1)
        next
      end
    end
    return false
  end
end

