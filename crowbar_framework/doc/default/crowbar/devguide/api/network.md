### Network API

The network API is used to manage networks.

#### Network CRUD: List

Lists the current networks.

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET  </td><td>crowbar/2.0/network/2.0/networks</td><td>N/A</td><td>JSON array of network IDs</td><td></td></tr>
</table>


**Output:**

    [
      1,
      2,
      3
    ]

#### Network CRUD: Show

Shows details about a selected network.

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET  </td><td>crowbar/2.0/network/2.0/networks/[id]</td><td>id is the network ID or name.</td><td>Details of the network in JSON format</td><td></td></tr>
</table>


**Output:**

    {
      "updated_at": "2012-08-24T14:40:26Z",
      "router": {
        "pref": 5,
        "ip": {
          "cidr": "192.168.122.1"
        }
      },
      "dhcp_enabled": "false",
      "subnet": {
        "cidr": "192.168.122.0/24"
      },
      "name": "public",
      "created_at": "2012-08-24T14:40:26Z",
      "ip_ranges": [
        {
          "start_address": {
            "cidr": "192.168.122.2"
          },
          "end_address": {
            "cidr": "192.168.122.49"
          },
          "name": "host"
        },
        {
          "start_address": {
            "cidr": "192.168.122.50"
          },
          "end_address": {
            "cidr": "192.168.122.127"
          },
          "name": "dhcp"
        }
      ],
      "id": 1,
      "conduit_id": 1
    }


#### Network CRUD: Create

Creates a new network.

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> POST  </td><td>crowbar/2.0/network/2.0/networks</td><td> json definition (see Node Show) </td><td> must be a legal object </td></tr>
</table>

    {
      "name":"public",
      "conduit_id":"1",
      "subnet":"192.168.122.0/24",
      "dhcp_enabled":"false",
      "ip_ranges":"{ "host": { "start": "192.168.122.2", "end": "192.168.122.49" }, "dhcp": { "start": "192.168.122.50", "end": "192.168.122.127" }}",
      "router_pref":"5",
      "router_ip":"192.168.122.1"
    }

Details:

* name - network name (must be letters and numbers, and must start with a letter)
* conduit_id - the ID or name of the conduit to associate with the network
* subnet - the subnet and netmask of the network in CIDR format
* dhcp_enabled - a boolean indicating if an external DHCP server should be used to provide IP addresses
* ip_ranges - the IP ranges to associate with the network (in JSON format)
* router_pref - the router preference
* router_ip - the IP of the router


#### Network CRUD: Update

Updates a new network.

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> PUT  </td><td>crowbar/2.0/network/2.0/networks/[id]</td><td></td><td></td><td></td></tr>
</table>


    {
      "id":"1",
      "conduit_id":"1",
      "subnet":"192.168.122.0/24",
      "dhcp_enabled":"false",
      "ip_ranges":"{ "host": { "start": "192.168.122.2", "end": "192.168.122.49" }, "dhcp": { "start": "192.168.122.50", "end": "192.168.122.127" }}",
      "router_pref":"5",
      "router_ip":"192.168.122.1"
    }

Details:

* id - the ID of the network to update
* conduit_id - the id or name of the conduit to associate with the network
* subnet - the subnet and netmask of the network in CIDR format
* dhcp_enabled - a boolean indicating if an external DHCP server should be used to provide IP addresses
* ip_ranges - the IP ranges to associate with the network (in JSON format)
* router_pref - the router preference
* router_ip - the IP of the router

#### Network CRUD: Delete

Deletes a network.

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> DELETE  </td><td>crowbar/2.0/network/2.0/networks/[id]</td><td> Database ID or name </td><td>HTTP error code 200 on success</td><td></td></tr>
</table>


No body.

**Output:**

None.
