# crowbar.yml

`<barclamp_name>/crowbar.yml` is a file that is required for every barclamp.  It contains metadata for the barclamp that includes the following high level sections:

* `barclamp:`
    * This contains information about your barclamp such as the name, version, copyright, and what barclamps your barclamp is dependent on.
* `crowbar:`
    * **TBD**
* `roles:`
    * Lists the roles that your barclamp provides, the attributes that they produce, and the dependencies between the roles.
* Multiple sections that list 3rd party software dependencies including:
    * `debs:`
        * This lists packages for debian based OSs that your barclamp requires.
    * `rpms:`
        * This lists packages for redhat based OSs that your barclamp requires.
    * `gems:`
        * This lists ruby gems that your barclamp requires.

## The `barclamp:` section contains the following parameters:
* `name:`
    * The name of your barclamp
* `display:`
    * The name of your barclamp that will appear in the GUI
* `version:`
    * The version number of your barclamp
* `license:`
    * **TBD**
* `copyright:`
    * **TBD**
* `api_version:`
    * **TBD**
* `api_version_accepts:`
    * **TBD**
* `requires:`
    * Contains a list of barclamps that your barclamp is dependent on
    * For example:
<pre>            requires:
               - openstack-base
               - messaging</pre>
