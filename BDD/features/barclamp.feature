Feature: Barclamp UI
  In order setup the sytem
  The system operator, Oscar
  wants to be able to select barclamps

  Scenario: Basic Screen
    When I go to the "barclamp" page
    Then I should see a heading {bdd:crowbar.i18n.barclamp.index.title}
      And I should see {bdd:crowbar.i18n.all}
      And I should see "Name"
      And I should see "Description"
      And I should see "Crowbar"
      And I should see "Deployer"
      And I should see "Provisioner"
      And I should see "polling is disabled"
      And there should be no translation errors
      
  Scenario: Modules Screen
    When I go to the "barclamp/crowbar" page
    Then I should see {bdd:crowbar.i18n.barclamp.index.members}
      And I should see {bdd:crowbar.i18n.all}
      And I should see "Name"
      And I should see "Description"
      And I should see "Crowbar"
      And I should see "Deployer"
      And I should see "Provisioner"
      And I should see "polling is disabled"
      And there should be no translation errors
      