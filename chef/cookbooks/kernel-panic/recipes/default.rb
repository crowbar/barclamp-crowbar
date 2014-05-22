# -*- encoding : utf-8 -*-
# Tell the kernel to reboot after 10 seconds on panic, if we want it to.
# This applies to linux distributions, not to Windows.
unless node[:platform] == "windows"
  if node["panic"] and node["panic"]["reboot"] == true
    if node["panic"]["timeout"]
      timeout = node["panic"]["timeout"]
    else
      timeout = 15
    end

    directory "create /etc/sysctl.d for reboot-on-panic" do
      path "/etc/sysctl.d"
      mode "755"
    end

    template "sysctl-reboot-on-panic.conf" do
      path "/etc/sysctl.d/80-reboot-on-panic.conf"
      mode "0644"
      variables ( { :timeout => timeout } )
    end

    bash "reload reboot-on-panic-sysctl" do
      code "/sbin/sysctl -e -q -p /etc/sysctl.d/80-reboot-on-panic.conf"
      action :nothing
      subscribes :run, resources(:template=> "sysctl-reboot-on-panic.conf"), :delayed
    end
  else
    bash "Stop rebooting on panic" do
      code <<-__EOC__
if [[ -f /etc/sysctl.d/80-reboot-on-panic.conf ]]; then
rm -f /etc/sysctl.d/80-reboot-on-panic.conf
fi
echo 0 >/proc/sys/kernel/panic
__EOC__
    end
  end
end
