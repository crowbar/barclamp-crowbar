## Making a Barclamps work with Crowbar

This section shows you how to make barclamps.

### Customization Locations

To create a barclamp, you must update many places with the name of your barclamp.  While this seems redundant inside the barclamp, it is essential when your barclamp gets merged into the larger Crowbar framework.  This often means that your barclamp tree will have one or two files inside a [barclamp] subdirectory.  That's normal and you'll be glad of it when you have to troubleshoot your barclamp later.

Locations of Crowbar Components in a typical barclamp

* `/bin` 
* `/chef` 
   * `/`
* `/crowbar_framework` 
   * `/app` - contains the code for the barclamp.  Crowbar does not require much code for basic operations, but stubs are required
      * `/controllers`
      * `/models`
      * `/views`
        * `/barclamp/[barclamp]/*.haml` - attribute & deployment view pages
      * `/asset`
   * `/BDD` - integrated testing framework (see DevGuide Testing)
   * `/doc` - integrated documentation frameowkr (see DevGuide documentation) 
      * `[barclamp].yml` - index of your barclamps additions to the documentation
      * `/default/[topic].md` - documentation files (can be in subdirectories)
   * `/db`   
      * `migrate` - required to create the barclamp record so Crowbar can find your barclamp in the database
        * `YYYYMMDDHHMMSS_barclamp_import_[barclamp].rb` - does the migration (see DevGuide barclamp meta data)
      
> Note: The good news is that that barclamp_model will start you off with all of these files populated!

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