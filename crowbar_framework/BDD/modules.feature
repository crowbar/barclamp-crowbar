Feature: Modules
  In order setup the sytem
  The system operator, Oscar
  wants to be able to select barclamps, create proposals, and apply them

  Scenario: View Barlcamps
    When I go to the "crowbar/modules/1.0" page
    Then I should see "Crowbar"
    Then I should see "Deployer"
    Then I should see "Dns"
    Then I should see "Ipmi"
    
  Scenario: Check Link Crowbar
    Given I went to the "crowbar/modules/1.0" page
    When I click on the "Crowbar" link
    Then I should see "Barclamp Details"
    Then I should see "Create Proposal"
    Then I should see "Active Proposals"
    Then I should see "default"
    Then I should see "All Proposals"    
    
  Scenario: %Direct Link Crowbar
    When I go to the "crowbar/show/1.0/crowbar" page
    Then I should see "Barclamp Details"
    Then I should see "Create Proposal"
    Then I should see "Active Proposals"
    Then I should see "default"
    Then I should see "All Proposals"    
    
    
    