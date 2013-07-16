Feature: DeploymentRole
  In order to track system deployed roles
  The system operator, Oscar
  wants to be able to manage deployment roles

  Scenario: REST DeploymentRole List
    When REST gets the {object:deployment_role} list
    Then the page returns {integer:200}
      
  Scenario: The Deployment Role page renders
    Given I am on the "deployment_roles" page
    Then I should see a heading {bdd:crowbar.i18n.deployment_roles.index.title}
      And there are no localization errors
