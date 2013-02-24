Feature: Jigs API
  In order to provision applications
  The system operator, Oscar
  wants to be able to use a configuration management database (like Chef or Puppet)

  Scenario: Jig List
    Given there is a jig "my_special_jig"
    When REST gets the {object:jig} list
    Then there should be a value "my_special_jig"
      And there should be a value "bddjig"
    Finally REST removes the jig "my_special_jig"

  Scenario: REST JSON check
    Given there is a jig "jig_json_test"
    When REST gets the jig "jig_json_test"
    Then the jig is properly formatted
    Finally REST removes the jig "jig_json_test"

  Scenario: REST Add Jig
    Given there is not a jig "jig_add_test"
    When REST adds the jig "jig_add_test"
    Then there is a jig "jig_add_test"
    Finally REST removes the jig "jig_add_test"

  Scenario: REST Delete Jig
    Given there is a jig "jig_delete_test"
    When REST deletes the jig "jig_delete_test"
    Then there is not a jig "jig_delete_test"
