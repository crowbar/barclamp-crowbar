Feature: CMDBs
  In order to provision applications
  The system operator, Oscar
  wants to be able to use a configuration management database (like Chef or Puppet)

  Scenario: CMDB List
    Given there is a cmdb "my-special-cmdb"
    When REST gets the cmdb list
    Then there should be a value "my-special-cmdb"
      And there should be a value "chef"
    Finally REST removes the node "my-special-cmdb"

  Scenario: REST Add CMDB
    Given there is a cmdb "cmdb-add-test"
    When REST gets the cmdb "cmdb-add-test"
    Then the cmdb is properly formatted

  Scenario: REST Add CMDB
    Given there is not a cmdb "cmdb-delete-test"
    When REST adds the cmdb "cmdb-delete-test"
    Then there is a cmdb "cmdb-delete-test"
    Finally REST removes the cmdb "cmdb-delete-test"
