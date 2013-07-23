Feature: NodeRole
  In order to track system node_roles
  The system operator, Oscar
  wants to be able to manage node_roles

  Scenario: REST List
    When REST gets the {object:node_role} list
    Then the page returns {integer:200}
      
  Scenario: The page renders
    When I go to the "node_roles" page
    Then I should see a heading {bdd:crowbar.i18n.node_roles.index.title}
      And I should see "system"
      And I should see "crowbar"
      And there are no localization errors

  Scenario: UI Index drill to snapshot
    Given I am on the "node_roles" page
    When I click on the "system" link
    Then I should see "system"
      And there are no localization errors

  Scenario: UI Index drill to role
    Given I am on the "node_roles" page
    When I click on the "crowbar" link
    Then I should see a heading "crowbar"
      And there are no localization errors
