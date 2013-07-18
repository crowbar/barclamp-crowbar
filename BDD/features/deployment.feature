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
      And there are no localization errors

  Scenario: Deployment UI click to Snapshot
    Skip ZEHICLE this should work when the system deployment gets active automatically
    Given I am on the "deployments" page
    When I click on the "Active system" link
    Then I should see "system"