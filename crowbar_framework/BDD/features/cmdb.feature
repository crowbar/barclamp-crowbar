Feature: CMDBs API
  In order to provision applications
  The system operator, Oscar
  wants to be able to use a configuration management database (like Chef or Puppet)

  Scenario: CMDB List
    Given there is a cmdb "my_special_cmdb"
    When REST gets the cmdb list
    Then there should be a value "my_special_cmdb"
      And there should be a value "bddcmdb"
    Finally REST removes the cmdb "my_special_cmdb"

  Scenario: REST JSON check
    Given there is a cmdb "cmdb_json_test"
    When REST gets the cmdb "cmdb_json_test"
    Then the cmdb is properly formatted
    Finally REST removes the cmdb "cmdb_json_test"

  Scenario: REST Add CMDB
    Given there is not a cmdb "cmdb_add_test"
    When REST adds the cmdb "cmdb_add_test"
    Then there is a cmdb "cmdb_add_test"
    Finally REST removes the cmdb "cmdb_add_test"

  Scenario: REST Delete CMDB
    Given there is a cmdb "cmdb_delete_test"
    When REST deletes the cmdb "cmdb_delete_test"
    Then there is not a cmdb "cmdb_delete_test"
