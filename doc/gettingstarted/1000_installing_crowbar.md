#Installing Crowbar#
Installation of the OS onto the admin node does not complete the Crowbar installation.  To complete the Crowbar installation follow these steps:

1. Log onto the admin node. The default username is *crowbar*, password is *crowbar*.

2. This step and substeps below are temporary.  Performing these steps changes the execution path to be equivalent to that of the production execution.  This is a necessary workaround to circumvent the developments auto-boot-strapping shortcut that will be go away shortly after this initial cut of the documentation is complete.  

  * sudo -i 
  * cd /tftpboot/ubuntu_dvd/extra
  * Open the install-crowbar-native.sh with your favorite editor and comment out the following lines before proceeding.

    * Lines 63-67 as shown below.  
    ```
      if [[ ! -f $DVD_PATH/sha1_passed ]]; then
        (cd $DVD_PATH && sha1sum -c sha1sums &>dev/null) || \
        die "SHA1sums do not match install is corrupt."
        >$DVD_PATH/sha1_passed
      fi
    ```
    * And lines starting at #227 (# Wait for puma to come to life) to the line # 282 or end of file. 
 
3. Run the install-crowbar shell script as root using the following procedures:
  * sudo -i
  * cd /opt/dell/bin
  * ./install-crowbar <Domain Name> --no-screen
  * Upon seeing the reboot message;  go forth and reboot the system 

4. Once the system comes back up,  use a web browser to connect to Crowbars main page at URL: HTTP://192.168.124.10:3000 You should be seeing a Crowbar Nodes screen with a "Configure System" button. 

5. Click on the "Configure System" button, which should take to the Initial System Configuration page.  
  * Click on the "Add network-admin Role" button to add it to the "Roles for Review" list.   
  * Click on the "Add <Domain Name> Node" button to add it to the  "Crowbar Admin Node" list. If for some reason you leave this page, you can always get back to here by selecting "Bootstrap" from the Utilities Menu drop-down list.   Also, be patient as the responsiveness of the actions may appear to be delayed after clicking the buttons.

  * Upon hitting the "Add <domain Name> Node"  annealer process will run.  One can watch the annealer processes by going to the Nodes option in the Nodes menu list, clicking on the <Domain Name> link.  Once the <Domain Name> page is shown, click on the "All Node Roles" button and one should see the states change as the annealr process steps through the nodes. 


###Crowbar and Operations Service Access###

```
- **SSH Service** - *SSH crowbar@192.168.124.10*, credentials are *crowbar*
- **Crowbar UI** - *http://192.168.124.10:3000*, credentials are *crowbar/crowbar*
- **Nagios UI** - *http://192.168.124.10/nagios3*, credentials are *nagiosadmin/password*
- **Ganglia UI** - *http://192.168.124.10/ganglia*, credentials are *nagiosadmin/password*
- **Chef UI** - *http://192.168.124.10:4040*, credentials are *admin/password*
```
