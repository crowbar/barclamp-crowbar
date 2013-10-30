Feature: Deployments
  In order to track system deploymenturation
  The system operator, Oscar
  wants to be able to manage deploymenturations

  Scenario: REST Deployment List
    When REST gets the {object:deployment} list
    Then the page returns {integer:200}
  
  Scenario: REST JSON check
    When REST gets the {object:deployment} "system"
    Then the {object:deployment} is properly formatted
    
  Scenario: The Deployment UI page renders
    Given I am on the "deployments" page
    Then I should see a heading {bdd:crowbar.i18n.deployments.index.title}
      And I should see "system"
      And I should see {apply:crowbar.i18n.deployments.index.parent}
      And there are no localization errors

  Scenario: Deployment UI click to Snapshot
    Skip rob needs to update
    Given I am on the "deployments" page
    When I click on the {lookup:deployment.d_name} link
    Then I should see "bravo_delta Proposed"

  Scenario: The system deployment has the system flag true
    When REST gets the {object:deployment} "system"
    Then key "system" should be "true"
      And key "name" should be "system"
      And key "parent_id" should be "null"

  Scenario: Can create new deployment from API
    Given there is not a {object:deployment} "bdd_deploy_test"
    When REST creates the {object:deployment} "bdd_deploy_test"
    Then there is a {object:deployment} "bdd_deploy_test"
    Finally REST removes the {object:deployment} "bdd_deploy_test"

  Scenario: New Deployment has a default snapshot
    Skip UI refactoring, please update
    Given there is a {object:deployment} "bdd_deploy_has_snap"
    When I go to the "snapshots/bdd_deploy_has_snap" page
    Then I should see "bdd_deploy_has_snap Proposed"
      And I should see a link to "bdd_deploy_has_snap"
    Finally REST removes the {object:deployment} "bdd_deploy_has_snap"

  Scenario: New Deployment renders in UI
    Given there is a {object:deployment} "bdd_deploy_showme"
    When I go to the "deployments/bdd_deploy_showme" page
    Then I should see "bdd_deploy_showme"
      And I should not see "something went wrong"
      And there should be no translation errors
    Finally REST removes the {object:deployment} "bdd_deploy_showme"

  Scenario: New Deployment is child of System
    Given there is a {object:deployment} "bdd_deploy_child"
    When REST gets the {object:deployment} "bdd_deploy_child"
    Then key "parent_id" should be "1"
    Finally REST removes the {object:deployment} "bdd_deploy_child"

  Scenario: Parent Deployment shown on UI
    Given there is a {object:deployment} "bdd_deploy_child_ui"
    When I go to the "deployments/bdd_deploy_child_ui" page
    Then I should see "Child of"
      And I should see a link to "system"
    Finally REST removes the {object:deployment} "bdd_deploy_child_ui"
  