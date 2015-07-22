Feature: Modules
  In order setup the sytem
  The system operator, Oscar
  wants to be able to select barclamps, create proposals, and apply them

  Scenario: View Barlcamps
    When I go to the "crowbar/modules/1.0" page
    Then I should see "Crowbar"
      And I should see "Deployer"
      And I should see "Dns"
      And I should see "Ipmi"
    
  Scenario: Check Link Crowbar
    Given I went to the "crowbar/modules/1.0" page
    When I click on the "Crowbar" link
    Then I should see "All Barclamps"
      And I should see "Crowbar"
      And I should see "Default"
      And I should see a link to "Edit"
    
  Scenario: Direct Link Crowbar
    When I go to the "crowbar/crowbar/1.0" page
    Then I should see "Crowbar Members"
      And I should see "Crowbar"
      And I should see "Provisioner"
      And I should see "Deployer"
      And I should see "Ipmi"
      And I should see "Network"
      And I should see "Dns"
      And I should see "Logging"
      And I should see "Ntp"
      And I should see "Test"

  Scenario: Crowbar Default Proposal Read Only
    When I go to the "crowbar/crowbar/1.0/default" page
    Then I should see "Crowbar: Default"
      And I should see "Proposal Attributes"
      And I should see "bios-settings:"
      And I should see "instances:"
      And I should see "node-usage:"
      And I should see "raid-settings:"
      And I should see "realm:"
      And I should see "users:"
      And I should see "web_port"
      And I should see a link to "Edit Proposal"
      
  Scenario: Crowbar Default Proposal Edit
    Given I am on the "crowbar/crowbar/1.0/default" page
    When I click on the "Edit Proposal" link
    Then I should see "Crowbar: Default"
      And I should see "Edit Proposal"
      And I should see "Attributes"
      And I should see "Deployment"
      And I should see "Raw"
      And I should see a button with "Deactivate"
      And I should see a button with "Apply"
      And I should see a button with "Save"
      
  Scenario: %Crowbar Default Proposal Edit Raw
    When I am on the "crowbar/crowbar/1.0/proposals/default?attr_raw=true" page
    Then I should see "Crowbar: Default"
      And I should see "Custom"
      And I should see "realm"
      And I should see "Crowbar - By selecting OK are agreeing to the License Agreement"
      And I should see "crowbar-status"
      And I should see "success"
