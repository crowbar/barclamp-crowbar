Feature: Deployments
  In order to track system deploymenturation
  The system operator, Oscar
  wants to be able to manage deploymenturations

  Scenario: Deployment List
    When REST gets the {object:deployment} list
    Then the page returns {integer:200}

  Scenario: The Deployment page renders
    Given I am on the "deployments" page
    Then I should see a heading {bdd:crowbar.i18n.deployments.index.title}
      And I should see "Default"
