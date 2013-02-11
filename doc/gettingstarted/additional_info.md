#Additional Information#

##OpenStack Barclamp Proposals Installation Order##
The OpenStack barclamp proposals installation order is important, but not readily apparent. It depends greatly upon your planned cloud usage (e.g., Nova compute  cluster, Swift storage cluster, etc.).

The examples below are just a few of the possible combinations of OpenStack barclamp proposals.

###Nova Compute Cluster###
	1. MySQL
	2. Keystone
	3. Nova-dashboard
	4. Nova
###Swift Storage Cluster###
	1. MySQL
	2. Keystone
	3. Swift
	4. Nova-dashboard
	5. Nova
###Combined Nova Compute/Swift Storage Cluster, with Glance Image Service and Tempest Tests###
	1. MySQL
	2. Keystone
	3. Swift
	4. Glance
	5. Nova-dashboard
	6. Nova
	7. Tempest