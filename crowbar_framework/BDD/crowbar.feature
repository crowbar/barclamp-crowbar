Feature: Crowbar, Check Core Navigation
  In order use the system
  The system operator, Oscar
  wants to be able to navigate around
  
  Scenario: Home Page Nav
    When I go to the home page
    Then I should see a menu for "Nodes"
      And I should see a menu for "Dashboard"
      And I should see a menu for "Bulk&nbsp;Edit"
      And I should see a menu for "Barclamps"
      And I should see a menu for "All&nbsp;Barclamps"
      And I should see a menu for "Crowbar"
      And I should see a menu for "Help"
      And I should see "CloudEdge Solution Team"
      And I should not see "Error"
    
  Scenario: Nodes Nav
    Given I am on the home page
    When I click on the "Nodes" menu item
    Then I should see "nodes available in the system"
      And I should see "admin"

  Scenario: Dashboard Nav
    Given I am on the home page
    When I click on the "Dashboard" menu item
    Then I should see "nodes available in the system"

  Scenario: Barclamps Nav
    Given I am on the home page
    When I click on the "Barclamps" menu item
    Then I should see "Barclamps"  
      And I should see "crowbar"
      And I should see "deployer"
      And I should see "provisioner"
      And I should see "dns"
      And I should see "ntp"

  Scenario: All Barclamps Nav
    Given I am on the home page
    When I click on the "All Barclamps" menu item
    Then I should see "All Barclamps"  
      And I should see "crowbar"
      And I should see "deployer"
      And I should see "provisioner"
      And I should see "dns"
      And I should see "ntp"

  Scenario: Crowbar Barclamps Nav
    Given I am on the home page
    When I click on the "Crowbar" menu item
    Then I should see "Crowbar Members"  
      And I should see "crowbar"
      And I should see "deployer"
      And I should see "provisioner"
      And I should see "dns"
      And I should see "ntp"
      And I should not see "Nova"
      And I should not see "Hadoop"
  
  Scenario: %Bulk Edit SubMenu
    Given I am on the home page
    When I click on the "Bulk Edit" menu item
    Then I should see "Bulk Edit (unallocated only)"
      And I should see "Allocate?"
      And I should see a button with "Save"
