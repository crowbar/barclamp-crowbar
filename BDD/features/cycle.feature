Feature: Cycle
  In order to track system cycles
  The system operator, Oscar
  wants to be able to manage cycles

  Scenario: REST List
    When REST gets the {object:cycle} list
    Then the page returns {integer:200}
    
  Scenario: The page renders
    Given I am on the "cycles" page
    Then I should see a heading {bdd:crowbar.i18n.cycles.index.title}
      And I should see "crowbar"
      And there are no localization errors
