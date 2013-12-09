#Installing Hadoop#
Use these instructions to install and configure a Hadoop cluster using Crowbar.

1. Configure and apply Hadoop barclamp proposals in Crowbar (this step also allocates nodes).
2. Verify nodes statuses (all nodes icons are green in Crowbar *Nodes* UI).
3. Troubleshoot the Chef client if necessary by running the following command on problematic nodes:

	`$ chef-client`
4. Verify nodes statuses in the Crowbar UI by navigating to *Nodes > Dashboard*, and then ensuring that all nodes' icons are green.
5. Print the *Nodes > Inventory* screen for later reference, when you select services. See *Service Selection* below.
6. Invoke the Cloudera Manager UI.

	a. Upload the Cloudera Manager license key, if applicable.

	b. Log out of Cloudera Manager.

	c. Restart the Cloudera Manager server in order for the license to take effect.

7. Open an SSH console on the node that has the *clouderamanager-server* (Cloudera Manager) role applied to it. The login credentials are *root*/*crowbar*. Then execute the following commands:

		$ cd /etc/init.d
		$ sudo service cloudera-scm-server restart
8. Log back into the Cloudera Manager UI.
9. Click the **Continue** button.

##Node Search##
1. Enter the IP range or hostname search pattern for all Hadoop cluster nodes. Cloudera Manager will search the cluster using this pattern and will consider any node with a Cloudera Manager agent process running on it as a valid Hadoop node candidate. For example:
	* *192.168.124.[ 80-90]* will attempt to discover all the nodes between 192.168.124.80 and 192.168.124.90
	* *192.168.124.8[1-3]* will attempt to discover 192.168.124.81, 192.168.124.82, and 192.168.124.83
	
	For additional information on Cloudera Manager search patterns, see the *search for hostnames and/or IP addresses using patterns* link on the Cloudera Manager user Interface.
2. Optionally, enter the host’s SSH Port. The default port is 22.
3. Click the **Search** button.

##Node Search Results##
1. Verify that all your Hadoop nodes have been discovered.
2. Make any cluster configuration adjustments by selecting or deselecting any checkboxes.

##Repository Configuration##
1. Click the **Install CDH On Selected Hosts** button.

	a. Select **CDH4** for installation.

	b. Select **Custom Repository for CDH**.

	c. Enter this URL - *http://192.168.124.10:8091/redhat-6.2/crowbar-extra/clouderamanager*
3. Select **None** to decline installing Impala.

	a. Or, select **Custom Repository** for Impala.

	b. Enter this URL - *[http://beta.cloudera.com/impala/redhat/6/x86_64/impala/0/](http://beta.cloudera.com/impala/redhat/6/x86_64/impala/0/ "Impala repository")*
5. Select **Custom Repository for Cloudera Manager**.

	a. Enter this URL - *http://192.168.124.10:8091/redhat-6.2/crowbar-extra/clouderamanager*
7. Leave the *GPG Key URL* field empty.
8. Click the **Continue** button.

##SSH Credentials##
1. Select **Login to all hosts as root**.

2. Select **All hosts accept same password**.

	a. Enter the SSH login password for the cluster. The default password is *crowbar*.

	b. Confirm the password.

3. Click the **Start Installation** button.
4. Accept the default settings for the SSH port and number of simultaneous installations.

##Package Install##

You will see bar graphs next to each node and the name of the package it is installing.

1. Wait for the installation process to complete.
2. Click the **Continue** button.

##Host Inspector##
The Cloudera Manager Host Inspector runs during this part of the installation process in order to validate the proper cluster configuration for the Hadoop installation.

1. Wait for this process to complete.
2. Click the **Run Again** button if you want to run the Host Inspector again.
3. Click the **Continue** button.

##Service Selection##
1. Select the services that you want to install. You can install All Services now or Core Services and optionally add additional services in the future.
2. Click the **Inspect Role Assignments** button to configure the Hadoop cluster services.

>**Important:** **Do not** select *Continue*, as this will give you the default role assignments, which may not be acceptable to you.

##Inspect Role Assignments #1##
1. Select the Cloudera Manager role assignments for Hadoop cluster deployment.
2. Click the **Continue** button.

##Inspect Role Assignments #2##
If you entered the Cloudera Manager License key, you will see this additional screen.

1. Select the role assignments for Hadoop add-ons services and monitoring services (Activity Monitor, Service Monitor Resource Manager).
	
	>**Note**: Best practice is to assign these roles to the Edge node.
2. Click the **Continue** button.

##Monitoring Database Setup##
If you entered the Cloudera Manager License key, you will see this additional screen.

1. Select **Use Embedded Database**.
2. You can leave the rest of the settings at default values unless you want to change them.
3. Click the **Test Connection** button to make sure you can connect to all the databases (required).
4. Click the **Continue** button.

##Review Configuration Changes##
If you entered the Cloudera Manager License key, you will see this additional screen.

1. Set the mail server hostname for alerts to *localhost*.
2. Set the mail server message recipients for alerts to *root*.
3. Click the **Continue** button.

##Cluster Services Initialization##
1. Wait for the Hadoop cluster installation process to complete.
2. Click the **Continue** button.

##Configuration Completion##
If the Hadoop configuration steps complete successfully, you will see the final Cloudera Manager confirmation screen.

1. Click the **Continue** button.

##Service Display##
This is the normal start-up screen after Cloudera Manager has completed the installation steps.
