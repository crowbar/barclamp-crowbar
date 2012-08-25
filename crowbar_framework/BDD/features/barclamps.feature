Feature: Barclamps
  In order setup the sytem
  The system operator, Oscar
  wants to be able to select barclamps

  Scenario: View Crowbar
    When I go to the "crowbar/crowbar/1.0" page
    Then I should see "Crowbar"
      And I should see "Deployer"
      And I should see "Dns"
      And I should see "Ipmi"
    
  Scenario: %Check Link Crowbar
    Given I went to the "crowbar" page
    When I click on the "Crowbar" link
    Then I should see "Barclamp Details"
      And I should see "Create Proposal"
      And I should see "Active Proposals"
      And I should see "default"
      And I should see "All Proposals"    
    
  Scenario: %Direct Link Crowbar
    When I go to the "crowbar/show/1.0/crowbar" page
    Then I should see "Barclamp Details"
      And I should see "Create Proposal"
      And I should see "Active Proposals"
      And I should see "default"
      And I should see "All Proposals"    
    
    
    
