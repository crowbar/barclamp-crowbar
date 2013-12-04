##Crowbar Barclamps

> NOTE: This is from the wiki and needs to be reviewed, RAH 11/9/2012

The Crowbar barclamp provides the roles and recipes to set up the barclamp framework.
It initializes the system, creates initial instances of other barclamps defined in its configuration, and creates the users to access the Crowbar API and GUI. Any number of barclamp instances can be started. Based upon their default  configurations, by default the system starts the following barclamps:

* Network
* Ganglia
* Nagios
* NTP
* DNS
* Provisioner
* Deployer
* IPMI
* Dell RAID
* BIOS

The initialization function of the Crowbar barclamp works exactly like the other barclamps. A proposal is created and can be committed during installation.

The main post-installation function is to provide the main transition entry point for the system. All barclamps' transition functions can be called directly, but the Crowbar barclamp calls them in an order specified in each barclamps' `crowbar.yml` file. The default unspecified priority is 1000. 

###Roles

The following node roles are defined:

* Crowbar 
  * Configures the system to run the barclamp framework (web application and other services)
  * Depends upon the apache2, rails, passenger, and utils cookbooks

###Scripts

The shared barclamp command line library is all the is provided to interact with the barclamp.

The following scripts are also provided.

<table border=0>
  <tr><th>Script</th><th>Description</td></tr>
  <tr>
    <td>crowbar</td>
    <td>Master control script for the command line interface</td></tr>
  <tr>
    <td>crowbar_crowbar</td>
    <td>The actual control script for the Crowbar barclamp</td></tr>
  <tr>
    <td>crowbar_watch_status</td>
    <td>Wrapper for script that watches the node state and node status</td></tr>
  <tr>
    <td>crowbar_node_state</td>
    <td>Displays the current provisioner state of the nodes</td></tr>
  <tr>
    <td>crowbar_node_status</td>
    <td>Displays the current Nagios state of the nodes</td></tr>
  <tr>
    <td>transition.sh</td>
    <td>A helper script that can be used to transition nodes</td></tr>
</table>

###Parameters

The Crowbar barclamp has a couple of list parameters.

<table border=0>
  <tr><th>Name</th><th>Default</th><th>Description</th></tr>
  <tr><td>instances</td><td>The starting barclamps using their default configurations</td><td>A map of barclamp names that reference a list of JSON files (default is special to mean to take the defaults) that represent starting barclamp instances to create</td></tr>
  <tr><td>users</td><td>A map of users - containing Crowbar</td><td>This map defines the users allowed to access Crowbar's GUI and REST API.</td></tr>
</table>

The users map contains a map. The key is the user name, and the rest of the required fields are:

<table border=0>
  <tr><th>Name</th><th>Description</th></tr>
  <tr><td>password</td><td>Clear text password of the user</td></tr>
  <tr><td>description</td><td>A description of the user.</td></tr>
</table>

###Operations

When the barclamp is committed, it uses a custom `apply_roles` function to ensure that the barclamps listed in the instances variable are created and committed.

Once running, the barclamp provides the global transition function that calls other barclamps as nodes transition. The barclamp is also responsible for creating new nodes and assigning them temporary names. The Deployer will change these, if needed, later in the node's life cycle. The transition function will also add the Crowbar configuration to the admin node as it transitions through the `discovered` state.

When starting a barclamp, use the following steps.

###Example (Using Swift)

####Proposals

   1. `crowbar swift proposal list` 
      - Output: Nothing, or the name of the current proposals (i.e., ... default, Default, etc...)
   2. `crowbar swift proposal show <Default> swift_default.txt` 
      - Output: creates the file `swift_default.txt` with the settings that are currently ready for deployment
      - Other things you can do with the file: 
         + Edit the file and change parameters. Once done you will need to import or edit the proposal.
   3. `crowbar swift proposal --file=swift_default.txt edit Default` 
      - Output: "Edited Default"
   4. `crowbar swift commit Default` 
      - Output: "Committed Default"

####Working with the Running Configuration

   1. `crowbar swift list` 
      - Lists current running configurations
   2. `crowbar swift show <Name>` 
      - Shows the config in question in stdout, you can use standard unix commands to send it to a file
   3. `crowbar swift --file=file.txt edit <Name>` 
      - Edit and commits the current running config
   4. `crowbar swift create default2` 
      - creates and commits a config using defaults

####Usage: `crowbar swift [options] <subcommands>`

* `--help` or `-h` - Displays help
* `--hostname <name or IP> or -n <name or IP>` - Specifies the destination server
* `--port <port>` or `-p <port>` - Specifies the destination server port
* `--debug` or `-d` - Turns on debugging information
* `--data <data>` - Used by create or edit as data (must be in JSON format)
* `--file <file>` - Used by create or edit as data when read from a file (must be in JSON format)
* `--timeout <seconds>` - Timeout, in seconds, for read HTTP reads
* `transition <name> <MAC> <state>` - Transitions a MAC to state
* `edit <name>` - Edits a new configuration
* `list` - Displays a list of current configurations
* `help` - Displays this page
* `delete <name>` - Deletes a configuration
* `element_node <name>` - Lists nodes that could be that element
* `elements` - Lists elements of a deployment
* `show <name>` - Shows a specific configuration
* `create <name>` - Creates a specific configuration
* `proposal` - Proposal sub-commands
* `commit <name>` - Commits a proposal to the active state
