##Installing OpenStack##
After node discovery, use these instructions to install and configure an OpenStack cluster using Crowbar.

1. Allocate the nodes in the Crowbar UI (*Nodes > Bulk Edit > Allocate*).
2. Configure and apply the OpenStack barclamp proposals in Crowbar.
3. Troubleshoot the Chef client if necessary by running the following command on problematic nodes:

		$ chef-client
4. Verify nodes statuses in the Crowbar UI (*Nodes > Dashboard*), ensuring that all nodes' icons are green.

**Note:** This can take some time; please be patient.