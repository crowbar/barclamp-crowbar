## Configuration Managers (aka Jigs)

Crowbar provides a pluggable model for interacting with nodes and other
infrastructure.  This model is known as the "Jig" function.

The core concept is that Crowbar has 1 or more Jigs that preform the
work of Crowbar via Jobs.

For initial Crowbar work, the primary Jig is the Chef jig in
Barclamp-Chef.

For testing, Crowbar provides a Test Jig that is included in the core
Crowbar barclamp.

### Inbound path

The jig can read data from the system and store it into attributes

### Outbound path

The jig can create roles for operation

The jig can set attributes for operations
