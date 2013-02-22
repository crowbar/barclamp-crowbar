Feature: Roles
  In order to track system roles
  The system operator, Oscar
  wants to be able to manage roles for instances

  Scenario: Roles List
    Skip until Rob completes this work
    When REST gets the {object:role} list
    Then there should be a value "crowbar"
      And there should be a value "private"

  Scenario: Crowbar Role
    Skip until Rob completes this work
    When REST gets the {object:role} "crowbar"
    Then there should be 
      And the {object:role} is properly formated
      
  Scenario: Private Role
    Skip until Rob completes this work
    When REST gets the {object:role} "private"
    Then there should be 
      And the {object:role} is properly formated
  
  