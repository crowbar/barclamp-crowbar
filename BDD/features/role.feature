Feature: Role
  In order to track system roles
  The system operator, Oscar
  wants to be able to manage roles

  Scenario: REST List
    When REST gets the {object:role} list
    Then the page returns {integer:200}
  
  Scenario: REST JSON check
    When REST gets the {object:role} "test-admin"
    Then the {object:role} is properly formatted
    
  Scenario: The page renders
    Given I am on the "roles" page
    Then I should see a heading {bdd:crowbar.i18n.roles.index.title}
      And I should see "test-admin"
      And there are no localization errors

  Scenario: Roles UI click to a snapshot
    Given I am on the "roles" page
    When I click on the "test-admin" link
    Then I should see "test-admin"

  Scenario: Role Page renders
    When I go to the "roles/test-admin" page
    Then I should see "test-admin"
      And there are no localization errors

  Scenario: Roles Page Drill to Role
    Given I am on the "roles" page
    When I click on the "test-admin" link
    Then I should see "test-admin"
      And there are no localization errors

  Scenario: Roles Page Drill to Jig
    Given I am on the "roles" page
    When I click on the "test" link
    Then I should see "test"
      And there are no localization errors

  Scenario: Roles Page Drill to Barclamp
    Given I am on the "roles" page
    When I click on the "test-admin" link
    Then I should see "test-admin"
      And there are no localization errors

  Scenario: BDD can turn off test delays
    Given I set the {object:role} "test-admin" property "test" to "false"
    When REST gets the {object:role} "test-admin"
    Then key "template:test" should be "false"
