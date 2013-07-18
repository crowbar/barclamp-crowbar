Feature: Snapshot
  In order to track system snapshots
  The system operator, Oscar
  wants to be able to manage snapshots

  Scenario: REST List
    When REST gets the {object:snapshot} list
    Then the page returns {integer:200}
  
  Scenario: REST JSON check
    When REST gets the {object:snapshot} "system"
    Then the {object:snapshot} is properly formatted
    
  Scenario: The page renders
    Given I am on the "snapshots" page
    Then I should see a heading {bdd:crowbar.i18n.snapshots.index.title}
      And I should see "system"
      And there are no localization errors

  Scenario: Snapshots UI click to a snapshot
    Given I am on the "snapshots" page
    When I click on the "system: Created Automatically by System" link
    Then I should see "system"

  Scenario: Snapshot Page renders
    When I go to the "snapshots/system" page
    Then I should see "system"
      And I should see {bdd:crowbar.i18n.snapshots.show.nodes}
      And there are no localization errors

  Scenario: Snapshot Page link to Roles 
    Given I am on the "snapshots/system" page
    When I click on the "crowbar" link
    Then I should see "crowbar"
      And there are no localization errors