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
    end
  else
    bash "Save reboot on panic" do
      code <<-__EOC__
if grep -q 'kernel.panic' /etc/sysctl.conf; then
sed -i '/^kernel.panic/ s/(kernel.panic = ).*/\1#{timeout}/' /etc/sysctl.conf
else
echo 'kernel.panic = #{timeout}' >>/etc/sysctl.conf
fi
__EOC__
    end
  end 
  bash "Reboot on panic" do
    code "echo #{timeout} >/proc/sys/kernel/panic"
  end
else
  bash "Stop rebooting on panic" do
    code <<-__EOC__
if [[ -f /etc/sysctl.d/80-reboot-on-panic.conf ]]; then
rm -f /etc/sysctl.d/80-reboot-on-panic.conf
fi
sed -i '/kernel.panic/ s/.*//' /etc/sysctl.conf
echo 0 >/proc/sys/kernel/panic
__EOC__
  end
end
