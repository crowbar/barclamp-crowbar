Feature: NodeRole
  In order to track system node_roles
  The system operator, Oscar
  wants to be able to manage node_roles

  Scenario: REST List
    When REST gets the {object:node_role} list
    Then the page returns {integer:200}
  
  Scenario: REST JSON check
    When REST gets the {object:node_role} "Default:crowbar-bddnode1.opencrowbar.org"
    Then the {object:node_role} is properly formatted
    
  Scenario: The page renders
    Given I am on the "node_roles" page
    Then I should see a heading {bdd:crowbar.i18n.node_roles.index.title}
      And I should see "Default:crowbar"
      And there are no localization errors
