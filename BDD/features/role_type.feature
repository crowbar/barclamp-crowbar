Feature: Role Types
  In order to track system roles
  The system operator, Oscar
  wants to be able to manage roles for instances

  Scenario: Role Types List
    When REST gets the {object:role_type} list
    Then there should be a value "crowbar"
      And there should be a value "private"

  Scenario: Crowbar Role
    When REST gets the {object:role_type} "crowbar"
    Then there should be a value "crowbar"
      And there should not be a value "private"
      And the {object:role_type} is properly formatted
      
  Scenario: Private Role
    When REST gets the {object:role_type} "private"
    Then the page returns {integer:200}
      And there should be a value "private"
      And the {object:role_type} is properly formatted

