Feature: Deployments
  In order to track system deploymenturation
  The system operator, Oscar
  wants to be able to manage deploymenturations

  Scenario: Deployment List
    When REST gets the {object:barclamp} "test" {object:deployment} list
    Then the page returns {integer:200}

  Scenario: Test Barclamp Exists
    Given I require a {object:barclamp} "test" 
    When REST gets the {object:barclamp} "test"
    Then the {object:barclamp} is properly formatted
    
  Scenario: Create Test-Default deployment
    Given I require a {object:barclamp} "test"
    When I propose a {object:deployment} "foobar" on the {object:barclamp} "test"
    Then the {object:deployment} is properly formatted
      And key "name" should be "foobar"
    Finally REST removes the {object:deployment} "foobar"

  Scenario: Created Template Shows up on UI
    Given I require a {object:barclamp} "test"
      And I propose a {object:deployment} "bdd_created_deployment" on the {object:barclamp} "test"
    When I go to the "barclamp" page
    Then I should see "bdd_created_deployment"
      And there should be no translation errors
    Finally REST removes the {object:deployment} "bdd_created_deployment"

  Scenario: Deployment shows up on list
    Given I require a {object:barclamp} "test"
      And I propose a {object:deployment} "deploy_list" on the {object:barclamp} "test"
    When REST gets the {object:barclamp} "test" {object:deployment} list
    Then the page returns {integer:200}
      And there should be a value "deploy_list"
    Finally REST removes the {object:deployment} "deploy_list"

  Scenario: Cannot create dups of a singleton deployment
    Given I require a {object:barclamp} "logging"
      And I propose a {object:deployment} "logging_deploy_1" on the {object:barclamp} "logging"
      And I propose a {object:deployment} "logging_deploy_2" on the {object:barclamp} "logging"
    When REST gets the {object:barclamp} "logging" {object:deployment} list
    Then the page returns {integer:200}
      And there should be a value "Default"
      And there should not be a value "logging_deploy_1"
      And there should not be a value "logging_deploy_2"
    Finally REST removes the {object:deployment} "logging_deploy_1"
      And REST removes the {object:deployment} "logging_deploy_2"

  Scenario: Can create dups of a multiple, non-intrinsic deployment
    Given I require a {object:barclamp} "test"
      And I propose a {object:deployment} "test_deploy_1" on the {object:barclamp} "test"
      And I propose a {object:deployment} "test_deploy_2" on the {object:barclamp} "test"
    When REST gets the {object:barclamp} "test" {object:deployment} list
    Then the page returns {integer:200}
      And there should not be a value "default"
      And there should be a value "test_deploy_1"
      And there should be a value "test_deploy_2"
    Finally REST removes the {object:deployment} "test_deploy_1"
      And REST removes the {object:deployment} "test_deploy_2"

  Scenario: The Deployment page renderse
    Given I am on the "barclamp" page
    When I click on the "Default" link
    Then I should see a heading "Crowbar Default deployment"
      And I should see a heading "crowbar role"
      And I should see {bdd:crowbar.i18n.deployments.show.deployment}
      And I should see {bdd:crowbar.i18n.deployments.show.attributes}