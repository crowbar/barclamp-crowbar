Feature: NodeRole
  In order to track system node_roles
  The system operator, Oscar
  wants to be able to manage node_roles

  Scenario: REST List
    When REST gets the {object:node_role} list
    Then the page returns {integer:200}

  Scenario: %REST update data
    Given I have a proposed {object:node_role}
    When update the {object:node_role} data to "{ \"bdd\":0 }"
    Then I should see a {object:node_role} with data
      And I should see a {object:node_role} with data key "bdd" 
      And I should see a {object:node_role} with data key "bdd" value {number:0}
      
  Scenario: The page renders
    When I go to the "node_roles" page
    Then I should see a heading {bdd:crowbar.i18n.node_roles.index.title}
      And I should see "system"
      And I should see "test-admin"
      And there are no localization errors

  Scenario: UI Index drill to snapshot
    Given I am on the "node_roles" page
    When I click on the "system" link
    Then I should see "system"
      And there are no localization errors

  Scenario: UI Index drill to role
    Given I am on the "node_roles" page
    When I click on the "test-admin" link
    Then I should see a heading "test-admin"
      And there are no localization errors
