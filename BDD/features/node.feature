Feature: Nodes
  In order check out the nodes
  The system operator, Oscar
  wants to be able to check the status of nodes

  Scenario: Nodes List
    When REST gets the {object:node} list
    Then the list should have an object with key "name" value {lookup:node.name}

  Scenario: REST JSON check
    When REST gets the {object:node} {lookup:node.name}
    Then the {object:node} is properly formatted
    
  Scenario: REST Can Delete
    Given REST creates the {object:node} "going.going.gone"
    When REST deletes the {object:node} "going.going.gone"
    Then I get a {integer:200} result
      And there is not a {object:node} "going.going.gone"
  
  Scenario: REST Get 404
    When REST gets the {object:node} "thisdoesnotexist"
    Then I get a {integer:404} error
    
  Scenario: Status Empty
    Skip TODO ZEHICLE disable during refactoring
    When AJAX requests node status on "all"
    Then key "sum" should be a number
      And there should be a key "state"
      And there should be a key "status"
      And there should be a key "groups"
      And there should be a key "i18n"
      And there should be a key "count"
      And key "[groups][0]" should contain "7" items

  Scenario: Status Non Nodes
    Skip TODO ZEHICLE disable during refactoring
    When AJAX requests node status on "0"
    Then key "sum" should be a number
      And there should be a key "state"
      And there should be a key "status"
      And there should be a key "groups"
      And there should be a key "i18n"
      And there should be a key "count"
      And key "count" should be "0"
      And key "[groups][0]" should contain "7" items
      
  Scenario: Node List
    Given there is a {object:node} "bdd-node-list.example.com"
    When REST gets the {object:node} list
    Then the list should have an object with key "name" value "bdd-node-list.example.com"
      And the list should have an object with key "name" value "bdd1.example.com"
      And the list should have an object with key "name" value "global-node.testing.com"
    Finally REST removes the {object:node} "bdd-node-list.example.com"
