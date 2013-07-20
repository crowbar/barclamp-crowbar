Feature: Jigs API
  In order to provision applications
  The system operator, Oscar
  wants to be able to use a configuration management database (like Chef or Puppet)

  Scenario: Jig List
    Given there is a {object:jig} "my_special_jig"
    When REST gets the {object:jig} list
    Then the list should have an object with key "name" value "my_special_jig"
      And the list should have an object with key "name" value "bddjig"
    Finally REST removes the {object:jig} "my_special_jig"

  Scenario: REST JSON check
    Given there is a {object:jig} "jig_json_test"
    When REST gets the {object:jig} "jig_json_test"
    Then the {object:jig} is properly formatted
    Finally REST removes the {object:jig} "jig_json_test"

  Scenario: REST Add Jig
    Given there is not a {object:jig} "jig_add_test"
    When REST creates the {object:jig} "jig_add_test"
    Then there is a {object:jig} "jig_add_test"
    Finally REST removes the {object:jig} "jig_add_test"

  Scenario: REST Delete Jig
    Given there is a {object:jig} "jig_delete_test"
    When REST deletes the {object:jig} "jig_delete_test"
    Then there is not a {object:jig} "jig_delete_test"

  Scenario: Jigs UI Page
    When I go to the "jigs" page
    Then I should see a heading {bdd:crowbar.i18n.jigs.index.title}
      And I should see "test"
      And I should see "script"
      And there should be no translation errors

  Scenario: Jigs UI Drill Down Test
    Given I am on the "jigs" page
    When I click on the "test" link
    Then I should see a heading "test"
      And there should be no translation errors

  Scenario: Jigs UI Drill Down Script
    Given I am on the "jigs" page
    When I click on the "script" link
    Then I should see a heading "script"
      And there should be no translation errors
    