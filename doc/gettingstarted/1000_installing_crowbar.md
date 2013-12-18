#Installing Crowbar#
Installation of the OS onto the admin node does not complete the Crowbar installation.  To complete the Crowbar installation follow these steps:

1. Log onto the admin node. The default username is *crowbar*, password is *crowbar*.
1. Run the `install-crowbar` shell script as root using the following procedures:

	a. `sudo -i`
	
	b. `cd /opt/dell/bin`
	
	c. `./install-crowbar <hostname.FQDN> --no-screen`
	
	d. A reboot message displays in the console "When the Crowbar installation is complete - Please reboot the Admin node" - do this after the next steps.


1. Once the system comes back up, use a web browser to connect to Crowbar's main page at [http://192.168.124.10:3000](http://192.168.124.10:3000).

	a. You should see a Crowbar Nodes screen with a *Configure System* button. 

1. Click on the **Configure System** button, which should take you to the *Initial System Configuration* page.  
	a. Click on the **Add network-admin Role** button to add the role to the *Roles for Reviewlist*.

	b. Click on the **Add <Domain Name> Node** button to add the node to the *Crowbar Admin Nodelist*, and begin the annealer process.
  
  	>If for some reason you leave this page, you can always get back to here by selecting "Bootstrap" from the Utilities Menu drop-down list.   Also, be patient as the responsiveness of the actions may appear to be delayed after clicking the buttons.

1. You can watch the annealer processes by:

	a. Going to the Nodes option in the Nodes menu list, and then clicking on the *<Domain Name>*link.

	b. Once the <Domain Name> page is shown, click on the **All Node Roles**button.

  You should see the states change as the annealer process steps through the nodes.


###Crowbar and Operations Service Access###

<table border="0">
<tr>
<th>Service</th>
<th>URL</th>
<th>Credentials</th>
</tr>
<tr>
<td><b>SSH Service</b></td>
<td><i>SSH crowbar@192.168.124.10</i></td>
<td><i>crowbar</i></td>
</tr>
<tr>
<td><b>Crowbar UI</b></td>
<td><i>http://192.168.124.10:3000</i></td>
<td><i>crowbar/crowbar</i></td>
</tr>
<tr>
<td><b>Nagios UI</b></td>
<td><i>http://192.168.124.10/nagios3</i></td>
<td><i>nagiosadmin/password</i></td>
</tr>
<tr>
<td><b>Ganglia UI</b></td>
<td><i>http://192.168.124.10/ganglia</i></td>
<td><i>nagiosadmin/password</i></td>
</tr>
<tr>
<td><b>Chef UI</b></td>
<td><i>http://192.168.124.10:4040</i></td>
<td><i>admin/password</i></td>
</tr>
</table>
