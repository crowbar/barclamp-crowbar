Feature: Barclamp API
  In order monitor the sytem
  The system operator, Oscar
  wants to be able to check the status of nodes

  Scenario: Barclamps List
    When REST gets the {object:barclamp} list
    Then there should be a value "crowbar"
      And there should be a value "provisioner"

  Scenario: Barclamps List with Network
    Skip remember to put the network back into the above test!!
    When REST gets the {object:barclamp} list
    Then there should be a value "network"

  Scenario: REST JSON check
    Skip TODO ZEHICLE disable during refactoring
    When REST gets the {object:barclamp} "crowbar"
    Then the {object:barclamp} is properly formatted
    
  Scenario: REST Cannot Delete
    When REST deletes the {object:barclamp} "crowbar"
    Then I get a {integer:405} error
  
  Scenario: REST Cannot Update
    When REST updates the {object:barclamp} "crowbar"
    Then I get a {integer:405} error

  Scenario: REST Get 404
    When REST gets the {object:barclamp} "thisdoesnotexist"
    Then I get a {integer:404} error
    
  Scenario: REST Cannot Create
    When REST creates the {object:barclamp} "foo"
    Then I get a {integer:405} error