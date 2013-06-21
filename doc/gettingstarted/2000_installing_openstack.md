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
| :-- | :----- |
| 1 | Database |
| 2 | Keystone |
| 3 | RabbitMQ |
| 4 | Glance |
| 5 | Cinder |
| 6 | Quantum |
| 7 | Nova |
| 8 | Nova-Dashboard |

###Swift Storage Cluster###
 
|  Install Order | Barclamp |
| :-- | :----- |
| 1 | Database |
| 2 | Keystone |
| 3 | Swift |
| 4 | Nova |
| 5 | Nova-dashboard |

###Combined Nova Compute/Swift Storage Cluster, with Glance Image Service and Tempest Tests###

|  Install Order | Barclamp |
| :-- | :----- |
| 1 | Database |
| 2 | Keystone |
| 3 | RabbitMQ |
| 4 | Swift |
| 5 | Glance |
| 6 | Cinder |
| 7 | Quantum |
| 8 | Nova |
| 9 | Nova-dashboard |
| 10 | Tempest |

