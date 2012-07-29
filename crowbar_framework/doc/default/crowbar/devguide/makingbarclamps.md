## Making a Barclamps work with Crowbar

This section shows you how to make barclamps.

#### Requested Sections
The following items are desired but not yet created in the documenation.

* Creating UI screens
* Adding Chef cookbooks
* ...

#### Importing a barclamp

_this is the manual process, there is a Utility for this in the UI_

Once you created a barclamp, you can import the barclamp into Crowbar & Chef.

Assuming that you already created the foo barclamp in /barclamps, here are the steps:

1.     From the Crowbar server, become the super admin: sudo –i
1.     Run the barclamp install script: /opt/dell/bin/barclamp_install /barclamps/foo
   1.         “/barclamps/foo” is the path to your barclamp. If could be anything!
   1.         The core barclamps are in /opt/dell/barclamps.
   1.         In a vm, you could mount a shared folder to access the barclamp (e.g.: /mnt/hgfs/barclamps)

Your barclamp should now show up in the Crowbar UI! You can also see it in Chef under the Crowbar databag.

While barclamps are generally safe to install multiple times, you can uninstall a barclamp using “barclamp_uninstall.rb /path/to/barclamp”