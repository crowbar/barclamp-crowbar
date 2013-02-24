Feature: Configuratons
  In order to track system configuration
  The system operator, Oscar
  wants to be able to manage configurations

  Scenario: Configuration List
    Skip until Rob completes this work
    When REST gets the {object:config} list
    Then there should be a value "crowbar: default"
      And there should be a value "network: default"
      And there should be a value "provisioner: default"

  Scenario: Test Barclamp Exists
    Skip until Rob completes this work
    Given there is a {object:barclamp} "test" 
    When REST gets the {object:barclamp} "test"
    Then the {object:barclamp} is properly formatted
    
  Scenario: Create Test-Default config
    Skip until Rob completes this work
    Given there is an {object:barclamp} "test"
    When create a {object:config} "default" on the {object:barclamp} "test"
    Then the {object:barclamp} is properly formatted
      And there should be a key "configs"
      And there should be a value "default"
    Finally REST removes the {object:config} "default" from the {object:barclamp} "test"
    
  Scenario: Test Test-Template has values
    Skip until Rob completes this work
    Given there is an {object:barclamp} "test"
    When I get the {object:barclamp} "test" template
    Then the {object:instance} is properly formatted
      And there should be a key "test"
      And there should be a value "test"
  
  Scenario: Create Test-Test
    Skip until Rob completes this work
    Given there is a {object:config} "test" on the {object:barclamp} "test"
    When I get the proposed {object:instance} "test" on the {object:barclamp} "test"
    Then the {object:instance} is properly formatted
      And there should be a key "test"
      And there should be a value "test"
    Finally REST removes the {object:config} "test" from the {object:barclamp} "test"
    
