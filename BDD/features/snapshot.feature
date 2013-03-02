Feature: Instances
  In order to track system roles
  The system operator, Oscar
  wants to be able to manage barclamp instances

  Scenario: Snapshots List
    Skip until Rob completes this work
    When REST gets the {object:instance} list
    Then there should be a value "crowbar"
      And there should be a value "private"

  Scenario: Crowbar Role
    Skip until Rob completes this work
    When REST gets the {object:instance} "crowbar"
    Then the {object:instance} is properly formated
      
  Scenario: Private Role
    Skip until Rob completes this work
    When REST gets the {object:instance} "private"
    Then the {object:instance} is properly formated
  
  