Feature: Crowbar, Check Core Navigation
  In order use the system
  The system operator, Oscar
  wants to be able to navigate around
  
  Scenario: Translation Check
    When I go to the home page
    Then there should be no translation errors
       And I should see "Digest"
       And I should see "Authenticated"
          
  Scenario: Home Page Nav
    When I go to the home page
    Then I should see a menu for "Nodes"
      And I should see a menu for "Barclamps"
      And I should see a menu for "Utilities"
      And I should see a menu for "Help"
      And I should see "CloudEdge Solutions Team"
      And I should not see "Error"
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

  Scenario: %Barclamps Nav
    Given I am on the home page
    When I click on the "Barclamps" menu item
    Then I should see "All Barclamps"  
      And I should see "crowbar"
      And I should see "deployer"
      And I should see "provisioner"
      And I should see "dns"
      And I should see "ntp"
      And there should be no translation errors

  Scenario: All Barclamps Nav
    Given I am on the home page
    When I click on the "All Barclamps" menu item
    Then I should see "All Barclamps" in the body
      And I should see "crowbar" in the body
      And I should see "deployer" in the body
      And I should see "provisioner" in the body
      And I should see "dns" in the body
      And I should see "ntp" in the body
      And there should be no translation errors

  Scenario: Crowbar Barclamps Nav
    Given I am on the home page
    When I click on the "Crowbar" menu item
    Then I should see "Crowbar Members" in the body  
      And I should see "crowbar" in the body
      And I should see "deployer" in the body
      And I should see "provisioner" in the body
      And I should see "dns" in the body
      And I should see "ntp" in the body
      And I should not see "Nova" in the body
      And I should not see "Hadoop" in the body
      And there should be no translation errors

  Scenario: Help Guide 
    Given I am on the home page
    When I click on the "Documentation" menu item
    Then I should see "System Documentation"
      And there should be no translation errors
    
  Scenario: %Help Deployment Guide PDF (legacy)
    Given I am on the home page
    When I click on the "Deployment" menu item
    Then I should download a PDF