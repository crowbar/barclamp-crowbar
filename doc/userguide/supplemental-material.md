#Supplemental Material#
##System Verification##
As a final step, it is important to verify that your deployment has succeeded. Crowbar provides limited feedback to confirm that the Chef recipes were successfully deployed.

You should consult the getting started guide and barclamps specific to your system for details on verification of deployment.
##Managing Barclamps##
This section briefly describes barclamps, and how to import barclamps. 
>![notes.png](graphics/notes.png "notes.png") For information about creating barclamps, please visit [https://github.com/crowbar/crowbar/wiki/Barclamp:-create-&-install-steps](https://github.com/crowbar/crowbar/wiki/Barclamp%3A-create-%26-install-steps "Creating barclamps") 
###Introduction###
A barclamp is a deployment module that is imported from its own code repository into the Crowbar framework. A barclamp cannot operate without Crowbar, but you do not have to create a unique build of Crowbar in order to create a barclamp.
>![notes.png](graphics/notes.png "notes.png") You must install Crowbar before importing barclamps.
###Importing a Barclamp###
Once you have created a barclamp, you can import the barclamp into Crowbar and Chef. Assuming that you already created the foo barclamp in /barclamps (see Creating a Barclamp), proceed as follows:

1. From the Crowbar server, become the super admin:

		sudo –i
2.	Run the barclamp install script:

		/opt/dell/bin/barclamp_install/barclamps/foo.tar.gz
>![notes.png](graphics/notes.png "notes.png") “/barclamps/foo.tar.gz” is the file name of your barclamp. You can name it anything you want. Note that the core barclamps are located in /opt/dell/barclamps.

Your barclamp should now appear in the Crowbar UI. You can also see it in Chef under the Crowbar data bag.

While barclamps are generally safe to install multiple times, you can uninstall a barclamp using: 	
	
		“barclamp_uninstall.rb /path/to/barclamp”.

###More Information###
Information on how to develop your own barclamp is available in the Developers Guide. If you are interested in creating, extending or contributing barclamps, please use one of the following contact methods.
##Support##
###Crowbar Support###
To obtain support for Crowbar:

- See the Crowbar wiki on GitHub: [https://github.com/dellcloudedge/crowbar/wiki](https://github.com/dellcloudedge/crowbar/wiki "Crowbar wiki")
- Gather log information.
- Email the Crowbar listserv — join us at: [https://lists.us.dell.com/mailman/listinfo/crowbar](https://lists.us.dell.com/mailman/listinfo/crowbar "Crowbar listserv")  

To help facilitate troubleshooting of the environment, a utility to gather logs has been provided. Use the Log Export function in the user interface as detailed above, or browse to http://< Admin_IP >:3000/support/logs. This creates a tar archive of the relevant logs and asks the user for a location to save the resulting archive.
>![notes.png](graphics/notes.png "notes.png") Depending on the size of the logs to be gathered, this utility may take a while to run.
##To Learn More ##
For more information about Crowbar, visit [http://www.dell.com/crowbar](http://www.dell.com/crowbar "Crowbar")

