## Making a barclamp

This section shows you how to make a barclamp.

### Create a crowbar.yml file

This section shows you how to setup a crowbar.yml file for your barclamp.

**TBD:** Document the minimum settings here.

Click [here](crowbar.yml.md) for full documentation on crowbar.yml.

### Creating the Chef side of the barclamp
#### Create the cookbooks
* Create the `<barclamp_name>/chef/cookbooks` directory
* Copy your custom cookbooks into subdirectories under `<barclamp_name>/chef/cookbooks`
* Create a symbolic link from `<barclamp_name>/chef-solo` to `<barclamp_name>/chef`
    * `cd <barclamp_name>`
    * `ln -s chef chef-solo`
    * `git add chef-solo`
#### Create the roles
* Create the `<barclamp_name>/chef/roles` directory
* In the `<barclamp_name>/chef/roles` directory, create a file named `<your_role_name>.rb` for each of the roles that your barclamp will have
* Each file should have content similar to the following:
    * `name "<your_role_name>"`
    * `description "Your Role Description"`
    * `run_list(`
    * `  "recipe[<your_cookbook_name>::<your_recipe_name>]"`
    * `)`
* Update the above appropriately for your roles

#### Use Berkshelf to resolve your cookbook dependencies
* Create a `Berksfile` file
    * Create a new file called `Berksfile` in the chef directory
    * Populate the file with the appropriate sources
* Run Berkshelf to pull in dependent cookbooks
    * `cd <barclamp_name>/chef`
    * `berks install`
        * Note, if `berks install` hangs, then try switching to http instead of git:
            * `git config --global url."http://".insteadOf git://`
* This will install all of the cookbooks into the Berkshelf
* Use Berkshelf to package the cookbooks
    * `berks package <your_cookbook_name>`
* Extract the cookbook package into the `<barclamp_name>/chef` directory
    * `gunzip <your_cookbook_name>.tar.gz`
    * `cd cookbooks`
    * `tar xvf ../<your_cookbook_name>.tar`
    * `rm ../<your_cookbook_name>.tar`
* Check in the collection of cookbooks
    * Make sure you are cded into the cookbooks directory, then run the following to check in the cookbooks
    * `ls | while read cookbook; do git add $cookbook; done`

### Testing your barclamp
#### Deploying your barclamp
* Note, the following instructions assume that the barclamp is resident on an installed Crowbar admin node
* After bringing up a Crowbar admin node, discover as many nodes as needed
* In the Crowbar GUI, navigate to Deployments->Deployments
* Enter the name for a deployment and click Add
* In the table in the History section, under the Snapshot column, click the name of the snapshot
* Select one of your roles from the pull down and click Add Role
* Repeat the above until all of your roles have been added
* Click Add Nodes
* Find the nodes you wish to deploy your roles to, for each change the Deployment to the deployment that you created, and click Save
* Navigate to Deployments->Deployments->your_deployment
* In the displayed table, click on the cells appropriately to deploy selected roles to selected nodes
* When done, click Commit
* At this point, the OS will be installed on your nodes, and then your roles will be deployed as you selected
* To watch the base Crowbar provisioning happening, navigate to Deployments->Deployments->system
* Once all the icons are green for your node, navigate to Deployments->Deployments->your_deployment to watch your roles deploying
* If an icon turns red, then an error has occurred.  Click the red icon to view the log from the Chef run, which should contain the error
#### Bug fix and retest cycle
* Make modifications to your recipes on the admin node in the `/opt/dell/barclamps/<barclamp_name>/chef/cookbooks` directory
* Navigate to Deployments->Deployments->your_deployment, click the red icon, then click Retry
* Refresh the browser to see the latest results


### Customization Locations

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

#### Create the WebApp Pieces

As a Rails app, you will need to following Rails naming conventions.  Say you name a barclamp, fred_rocks.  This means that the camelized name would be FredRocks.  

#### Create the WebApp Controller

The controller file lives in crowbar_framework/app/controllers.  It should be named bcname_controller.rb.  bcname is the lower case name of with the barclamp.  The class name should be the camelized name of the barclamp with Controller and it should inherit from BarclampController.

It should look like this:
class FredRocksController < BarclampController
end

For normal operation and to provide the default APIs, nothing else need to be provided.

#### Create the WebApp Service Object

The service object lives in the crowbar_framework/app/models directory.  It is usually named bcname_service.rb.  bcname is the lowercase version of the name.  The class name should be the camelized version of the barclamp name with Service and it should inherit from ServiceObject.

It should look like this:
class FredRocksService < ServiceObject
end

For normal operation and to provide the default APIs, nothing else need to be provided.

Within this class, the following routines can be overridden:
* create_proposal(name)
* transition(name, inst, state)
* apply_role_pre_chef_call(old_config, new_config, all_nodes)
* proposal_dependencies(proposal_config)
* validate_proposal_elements(proposal_elements)
* validate_proposal(proposal)

These can be overriden, but usually aren't.  They represent the basic API calls that have complex actions within the barclamp.
* apply_role(new_proposal_config, in_queue)
* destroy_active(prop_name)
* dequeue_proposal(prop_name)
* proposal_create(params)
* proposal_edit(params)
* proposal_delete(prop_name)
* proposal_commit(prop_name, in_queue)

#### Creating Barclamp Views

Crowbar has pre-wired routes that allow barclamps to create custom view pages without changing the routes file.  These views must conform to the following pattern in the Barclamp Controller object.

<table>
  <tr><th>Method</th><th>Route</th><th>Path</th></tr>
  <tr>
    <td>node</td>
    <td>barclamp_node_path(:controller=>_)</td>
    <td>/barclamp/[:controller]/node/[:id]</td></tr>
  <tr>
    <td>network</td>
    <td>barclamp_network_path(:controller=>_)</td>
    <td>/barclamp/[:controller]/network/[:id]</td></tr>
  <tr>
    <td>util</td>
    <td>barclamp_util_path(:controller=>_)</td>
    <td>/barclamp/[:controller]/util/[:id]</td></tr>
</table>

> All these methods can optionally take :id as long as :id is limited to valid ID/Names

> Please review the 'adding-navigation` DevGuide topic for information about including the view in the menu.

#### Importing a barclamp

_this is the manual process, there is a Utility for this in the UI_

Once you created a barclamp, you can import the barclamp into Crowbar & Chef.

Assuming that you already created the foo barclamp in /barclamps, here are the steps:

1.     From the Crowbar server, become the super admin: sudo -i
1.     Run the barclamp install script: /opt/dell/bin/barclamp_install /barclamps/foo
   1.         barclamps/foo is the path to your barclamp. If could be anything!
   1.         The core barclamps are in /opt/dell/barclamps.
   1.         In a vm, you could mount a shared folder to access the barclamp (e.g.: /mnt/hgfs/barclamps)

Your barclamp should now show up in the Crowbar UI! You can also see it in Chef under the Crowbar databag.

While barclamps are generally safe to install multiple times, you can uninstall a barclamp using barclamp_uninstall.rb /path/to/barclamp
