Feature: Barclamps API
  In order setup the sytem
  The system operator, Oscar
  wants to be able to select barclamps

  Scenario: Barclamps List
    When REST gets the {object:barclamp} list
    Then there should be a value "crowbar"
      And there should be a value "network"
      And there should be a value "provisioner"

  Scenario: REST JSON check
    When REST gets the {object:barclamp} "crowbar"
    Then the {object:barclamp} is properly formatted