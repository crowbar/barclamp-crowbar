Feature: Navigation, Check Core Navigation
  In order use the system
  The system operator, Oscar
  wants to be able to navigate around
  
  Scenario: Translation Check
    When I go to the home page
    Then there should be no translation errors

  Scenario: Top Nav Renders
    Skip until network menus are working again
    When I go to the home page
    Then I should not see "Render Error"
          
  Scenario: Home Page Nav
    When I go to the home page
    Then I should see a menu for {bdd:crowbar.i18n.nav.dashboard}
      And I should see a menu for "Barclamps"
      And I should see a menu for "Utilities"
      And I should see a menu for "Help"
      And I should see "CloudEdge Solutions Team"
      And I should not see "something went wrong"
      And there should be no translation errors

  Scenario: Nodes Nav
    Given I am on the home page
    When I click on the "Nodes" menu item
    Then I should see a menu for "Dashboard"
      And I should see a menu for "Bulk&amp;Edit"
      And I should see "nodes available in the system"
      And there should be no translation errors

  Scenario: Dashboard Nav
    Given I am on the home page
    When I click on the "Dashboard" menu item
    Then I should see "nodes available in the system"
      And there should be no translation errors

  Scenario: Help Guide 
    Given I am on the home page
    When I click on the "Help" menu item
    Then I should see "System Documentation" in the body
      And I should see "(Master Index)" in the body
      And there should be no translation errors
