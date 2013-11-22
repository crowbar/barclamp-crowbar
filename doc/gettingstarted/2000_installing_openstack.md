#Installing OpenStack#
After node discovery, use these instructions to install and configure an OpenStack cluster using Crowbar.

1. Allocate the nodes in the Crowbar UI by navigating to *Nodes > Bulk Edit*, and then selecting **Allocate**.
2. Configure and apply the OpenStack barclamp proposals in Crowbar. See *About OpenStack Barclamps* below.
3. Troubleshoot the Chef client if necessary by running the following command on problematic nodes:

	`$ chef-client`
4. Verify nodes statuses in the Crowbar UI by navigating to *Nodes > Dashboard*, and then ensuring that all nodes' icons are green.

>**Note:** This can take some time; please be patient.

##About OpenStack Barclamps##
The OpenStack barclamp proposals installation order is important, but not readily apparent. It depends greatly upon your planned cloud usage (e.g., Nova compute  cluster, Swift storage cluster, etc.).

The examples below represent just a few of the possible combinations of OpenStack barclamp proposals. They are not intended to be definitive.

###Nova Compute Cluster###

<table border="0">
<tr>
<th>Install Order</th>
<th>Barclamp</th>
</tr>
<tr>
<td>1</td>
<td>Database</td>
</tr>
<tr>
<td>2</td>
<td>Keystone</td>
</tr>
<tr>
<td>3</td>
<td>RabbitMQ</td>
</tr>
<tr>
<td>4</td>
<td>Glance</td>
</tr>
<tr>
<td>5</td>
<td>Cinder</td>
</tr>
<tr>
<td>6</td>
<td>Neutron</td>
</tr>
<tr>
<td>7</td>
<td>Nova</td>
</tr>
<tr>
<td>8</td>
<td>Nova-Dashboard</td>
</tr>
</table>

###Swift Storage Cluster###
 
<table border="0">
<tr>
<th>Install Order</th>
<th>Barclamp</th>
</tr>
<tr>
<td>1</td>
<td>Database</td>
</tr>
<tr>
<td>2</td>
<td>Keystone</td>
</tr>
<tr>
<td>3</td>
<td>Swift</td>
</tr>
<tr>
<td>4</td>
<td>Nova</td>
</tr>
<tr>
<td>5</td>
<td>Nova-Dashbaord</td>
</tr>
</table>

###Combined Nova Compute/Swift Storage Cluster, with Glance Image Service and Tempest Tests###

<table border="0">
<tr>
<th>Install Order</th>
<th>Barclamp</th>
</tr>
<tr>
<td>1</td>
<td>Database</td>
</tr>
<tr>
<td>2</td>
<td>Keystone</td>
</tr>
<tr>
<td>3</td>
<td>RabbitMQ</td>
</tr>
<tr>
<td>4</td>
<td>Swift</td>
</tr>
<tr>
<td>5</td>
<td>Glance</td>
</tr>
<tr>
<td>6</td>
<td>Cinder</td>
</tr>
<tr>
<td>7</td>
<td>Neutron</td>
</tr>
<tr>
<td>8</td>
<td>Nova</td>
</tr>
<tr>
<td>9</td>
<td>Nova-Dashboard</td>
</tr>
<tr>
<td>10</td>
<td>Tempest</td>
</tr>
</table>

