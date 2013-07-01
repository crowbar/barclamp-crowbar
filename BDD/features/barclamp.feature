Feature: Barclamp UI
  In order setup the sytem
  The system operator, Oscar
  wants to be able to select barclamps

  Scenario: Basic Screen
    When I go to the "barclamps" page
    Then I should see a heading {bdd:crowbar.i18n.barclamps.index.title}
      And I should see "crowbar"
      And I should see "deployer"
      And I should see "provisioner"
      And there should be no translation errors
      
