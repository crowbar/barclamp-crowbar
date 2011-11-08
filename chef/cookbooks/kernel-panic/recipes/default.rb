# Tell the kernel to reboot after 10 seconds on panic, if we want it to.
if node["panic"] and node["panic"]["reboot"] == true
  if node["panic"]["timeout"]
    timeout = node["panic"]["timeout"]
  else
    timeout = 15
  end
  if ::File.exists?("/etc/sysctl.d") and ::File.directory?("/etc/sysctl.d")
    bash "Save reboot on panic" do
      code "echo 'kernel.panic = #{timeout}' >/etc/sysctl.d/80-reboot-on-panic.conf"
      not_if "test -f /etc/sysctl.d/80-reboot-on-panic.conf"
    end
  elsif ::File.exists?("/etc/sysctl.conf")
    bash "Save reboot on panic" do
      code "echo 'kernel.panic = #{timeout}' >>/etc/sysctl.conf"
      not_if "grep -q 'kernel.panic' /etc/sysctl.conf"
    end
  end
  
  bash "Reboot on panic" do
    code "echo #{timeout} >/proc/sys/kernel/panic"
    only_if "test -f /proc/sys/kernel/panic"
  end
end
