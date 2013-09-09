Feature: NodeRole
  In order to track system node_roles
  The system operator, Oscar
  wants to be able to manage node_roles

  Scenario: REST List
    When REST gets the {object:node_role} list
    Then the page returns {integer:200}

  Scenario: REST can add a node to a role
    Given there is a {object:deployment} "bdd_deploy_add_role"
      And there is a {object:role} "bdd_role_add_node"
      And there is a {object:node} "bdd-add-me-to-role.cr0wbar.com"
    When I add {object:node} "bdd-add-me-to-role.cr0wbar.com" to {object:deployment} "bdd_deploy_add_role" in {object:role} "bdd_role_add_node"
    Then I get a {integer:200} result
      And key "state" should be {apply:crowbar.state.proposed}
      And the {object:node_role} is properly formatted
    Finally REST removes the {object:deployment} "bdd_deploy_add_role"
      And REST removes the {object:role} "bdd_role_add_node"
      And REST removes the {object:node} "bdd-add-me-to-role.cr0wbar.com"

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
