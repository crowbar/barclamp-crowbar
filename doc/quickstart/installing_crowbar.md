##Installing Crowbar##
The initial admin node installation does not complete the Crowbar installation, to allow the network configuration to be customized. After any customizations have been made, the final Crowbar installation can be completed. The networks cannot be reconfigured after Crowbar is installed.

To complete the Crowbar installation:

1. Log onto the admin node. The default username is *crowbar*, password is *crowbar*.
2. Verify or edit the network configuration file in your editor of choice.  For example:

		$ vi /opt/dell/barclamps/network/chef/data_bags/crowbar/bc-template-network.json
3. Enter the following commands:

		$ sudo –i
		$ cd /tftpboot/ubuntu_dvd/extra
		$ ./install systemname.yourdomain.com

The Crowbar installation will be started in a screen session. You can attach to this session to follow the install process. The install logs are written to */var/log*, and can be checked if there are any errors during the install process:

		$ tail -50f /var/log/install.log

The main cause of errors at this point is usually syntax errors caused while modifying the network configuration. If an error occurs, check the log files, fix any syntax errors, and then restart the Crowbar install process.

**Note:** This can take some time; please be patient.

When the Crowbar installation completes, the admin node will remain at a shell prompt. At this point, all Crowbar and operations services have started.
###Crowbar and Operations Service Access###

- **SSH Service** - *SSH crowbar@192.168.124.10*, credentials are *crowbar*
- **Crowbar UI** - *http://192.168.124.10:3000*, credentials are *crowbar/crowbar*
- **Nagios UI** - *http://192.168.124.10/nagios3*, credentials are *nagiosadmin/password*
- **Ganglia UI** - *http://192.168.124.10/ganglia*, credentials are *nagiosadmin/password*
- **Chef UI** - *http://192.168.124.10:4040*, credentials are *admin/password*