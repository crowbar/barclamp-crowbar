#Installing OpenStack#
After node discovery, use these instructions to install and configure an OpenStack cluster using Crowbar.

1. Allocate the nodes in the Crowbar UI by navigating to *Nodes > Bulk Edit*, and then selecting *Allocate*.
2. Configure and apply the OpenStack barclamp proposals in Crowbar. See **About OpenStack Barclamps** below.
3. Troubleshoot the Chef client if necessary by running the following command on problematic nodes:

		$ chef-client
4. Verify nodes statuses in the Crowbar UI by navigating to *Nodes > Dashboard*, and then ensuring that all nodes' icons are green.

**Note:** This can take some time; please be patient.

##About OpenStack Barclamps##
The OpenStack barclamp proposals installation order is important, but not readily apparent. It depends greatly upon your planned cloud usage (e.g., Nova compute  cluster, Swift storage cluster, etc.).

The examples below represent just a few of the possible combinations of OpenStack barclamp proposals. They are not intended to be definitive.

###Nova Compute Cluster###

| Install Order | Barclamp |
|-- | ----- |
| 1 | MySQL |
| 2 | Keystone |
| 3 | Nova-dashboard |
| 4 | Nova |

###Swift Storage Cluster###
 
|  Install Order | Barclamp |
|-- | ----- |
| 1 | MySQL |
| 2 | Keystone |
| 3 | Swift |
| 4 | Nova-dashboard |
| 5 | Nova |

###Combined Nova Compute/Swift Storage Cluster, with Glance Image Service and Tempest Tests###

|  Install Order | Barclamp |
|-- | ----- |
| 1 | MySQL |
| 2 | Keystone |
| 3 | Swift |
| 4 | Glance |
| 5 | Nova-dashboard |
| 6 | Nova |
| 7 | Tempest |

