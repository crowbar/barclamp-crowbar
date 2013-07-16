Feature: Role
  In order to track system roles
  The system operator, Oscar
  wants to be able to manage roles

  Scenario: REST List
    When REST gets the {object:role} list
    Then the page returns {integer:200}
  
  Scenario: REST JSON check
    When REST gets the {object:role} "crowbar"
    Then the {object:role} is properly formatted
    
  Scenario: The page renders
    Given I am on the "roles" page
    Then I should see a heading {bdd:crowbar.i18n.roles.index.title}
      And I should see "crowbar"
      And there are no localization errors

  Scenario: Roles UI click to a snapshot
    Given I am on the "roles" page
    When I click on the "crowbar" link
    Then I should see "crowbar"

  Scenario: Role Page renders
    When I go to the "roles/crowbar" page
    Then I should see "crowbar"
      And there are no localization errors

  Scenario: Roles Page Drill to Role
    Given I am on the "roles" page
    When I click on the "test-admin" link
    Then I should see "test-admin"
      And there are no localization errors

  Scenario: Roles Page Drill to Jig
    Given I am on the "roles" page
    When I click on the "script" link
    Then I should see "script"
      And there are no localization errors

  Scenario: Roles Page Drill to Barclamp
    Given I am on the "roles" page
    When I click on the "crowbar" link
    Then I should see "crowbar"
      And there are no localization errors
