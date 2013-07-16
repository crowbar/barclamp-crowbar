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
