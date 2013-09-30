Feature: Nodes
  In order check out the nodes
  The system operator, Oscar
  wants to be able to check the status of nodes

  Scenario: UI Node List
    When I go to the "nodes" page
    Then I should see {bdd:crowbar.i18n.nodes.index.title}
      And I should see {lookup:crowbar.node_name}
      And there should be no translation errors

  Scenario: UI Node List Click to Node
    Given I am on the "nodes" page
    When I click on the {lookup:crowbar.node_name} link
    Then I should see {lookup:crowbar.node_name}
      And there should be no translation errors

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
      And the list should have an object with key "name" value {lookup:crowbar.node_name}
    Finally REST removes the {object:node} "bdd-node-list.example.com"

  Scenario: Node Alive Settable Default False
    Given there is a {object:node} "bdd-alive-false.example.com"
    When REST gets the {object:node} "bdd-alive-false.example.com"
    Then key "alive" should be "false"
      And the {object:node} is properly formatted
    Finally REST removes the {object:node} "bdd-alive-false.example.com"

  Scenario: Node Alive Settable
    Given there is a {object:node} "bdd-alive-set.example.com"
    When REST sets the {object:node} "bdd-alive-set.example.com" {atom:alive} state to be "true"
    Then key "alive" should be "true"
      And the {object:node} is properly formatted
    Finally REST removes the {object:node} "bdd-alive-set.example.com"

  Scenario: Node Available Settable Default True
    Given there is a {object:node} "bdd-available-false.example.com"
    When REST gets the {object:node} "bdd-available-false.example.com"
    Then key "available" should be "true"
      And the {object:node} is properly formatted
    Finally REST removes the {object:node} "bdd-available-false.example.com"

  Scenario: Node Available Settable
    Given there is a {object:node} "bdd-available-set.example.com"
    When REST sets the {object:node} "bdd-available-set.example.com" {atom:available} state to be "false"
    Then key "available" should be "false"
      And the {object:node} is properly formatted
    Finally REST removes the {object:node} "bdd-available-set.example.com"

  Scenario: Node UI shows alive
    Given there is a {object:node} "bdd-alive-ui.example.com"
    When I go to the "nodes/bdd-alive-ui.example.com" page 
    Then I should see {bdd:crowbar.i18n.common.state.dead}
    Finally REST removes the {object:node} "bdd-alive-ui.example.com"

  Scenario: Nodes UI shows alive
    Given there is a {object:node} "bdd-alive-ui.example.com"
    When I go to the "nodes" page 
    Then I should see {bdd:crowbar.i18n.common.state.dead}
    Finally REST removes the {object:node} "bdd-alive-ui.example.com"

  Scenario: Node UI shows reserved
    Given there is a {object:node} "bdd-reserved-ui.example.com"
      And REST sets the {object:node} "bdd-reserved-ui.example.com" {atom:available} state to be "false"
    When I go to the "nodes/bdd-reserved-ui.example.com" page 
    Then I should see {bdd:crowbar.i18n.common.state.reserved}
    Finally REST removes the {object:node} "bdd-reserved-ui.example.com"

  Scenario: Nodes UI shows reserved
    Given there is a {object:node} "bdd-reserved-ui1.example.com"
      And REST sets the {object:node} "bdd-reserved-ui1.example.com" {atom:available} state to be "false"
    When I go to the "nodes" page 
    Then I should see {bdd:crowbar.i18n.common.state.reserved}
    Finally REST removes the {object:node} "bdd-reserved-ui1.example.com"
