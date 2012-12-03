Feature: Nodes
  In order check out the nodes
  The system operator, Oscar
  wants to be able to check the status of nodes

  Scenario: Status Empty
    When AJAX requests the "2.0/status/node" page
    Then key "sum" should be a number
      And there should be a key "state"
      And there should be a key "status"
      And there should be a key "groups"
      And there should be a key "i18n"
      And there should be a key "count"
      And key "[groups][0]" should contain "7" items

  Scenario: Status Non Nodes
    When AJAX requests the "2.0/status/node/0" page
    Then key "sum" should be a number
      And there should be a key "state"
      And there should be a key "status"
      And there should be a key "groups"
      And there should be a key "i18n"
      And there should be a key "count"
      And key "count" should be "0"
      And key "[groups][0]" should contain "7" items
      
  Scenario: Node List
    Given there is a node "bdd-node-list.example.com"
    When REST gets the node list
    Then there should be a value "bdd-node-list.example.com"
      And there should be a value "bdd1.example.com"
      And there should be a value "global-node.testing.com"
    Finally REST removes the node "bdd-node-list.example.com"
    
  Scenario: Node Detail
    When I go to the "node/bdd1.example.com" page
    Then I should see "Full Name"
      And I should see "State"
      And I should see "Uptime"
      And I should see "Switch"
      And I should see "MAC Address"
      And I should see "Allocated"
      And I should see "Description"
      And I should see "Memory"
      And I should see "Disk Drives"
      And I should see a link to "Edit"

  Scenario: Node Detail REST
    When REST gets the node "bdd1.example.com"
    Then key "fingerprint" should be a number
      And there should be a key "state"
      And there should be a key "name"
      And there should be a key "description"
      And there should be a key "created_at"
      And there should be a key "os_id"
      And there should be a key "id"
      And key "id" should be a number
      And there should be a key "order"
      And key "order" should be a number
      And there should be a key "updated_at"
      And the node is properly formatted

  Scenario: Node Attribute List Works
    Given {object:node} "bdd1.example.com" has {object:attribute} "bddtest1"
    When REST gets the node-attribute list for "bdd1.example.com"
    Then there should be a value "bddtest1"
    Finally REST unassigns {object:attribute} "bddtest1" from {object:node} "bdd1.example.com"

  Scenario: %Node Attribute Get Events
    Given {object:node} "node1.attribute.com" with {object:attribute} "bdd1test" has value "foo"
    When REST gets the {object:node} "node1.attribute.com" with {object:attribute} "bdd1test"
    Then there is a value "foo"
    Finally REST removes {object:node} "node1.attribute.com"
      And REST removes {object:attribute} "bdd1test"  
      
  Scenario: Node Assign Attribute
    Given there is a {object:node} "node2.attribute.com"
      And there is a {object:attribute} "bdd2test"
    When REST assigns {object:attribute} "bdd2test" to {object:node} "node2.attribute.com"
    Then the result is a valid node-attribute json
      And {object:node} "node2.attribute.com" has {object:attribute} "bdd2test"
    Finally REST unassigns {object:attribute} "bddtest1" from {object:node} "bdd1.example.com"
      And REST removes {object:node} "node2.attribute.com"
      And REST removes {object:attribute} "bdd2test"  
  
  Scenario: %Node Remove Attribute
    Given there is a {object:node} "node3.attribute.com" with {object:attribute} "bdd3test"
    When REST unassigns {object:attribute} "bdd3test" from {object:node} "node3.attribute.com"
    Then the page returns "200" result
    Finally REST removes {object:node} "node3.attribute.com"
      And REST removes {object:attribute} "bdd3test"  

  Scenario: %Node cannot Update Attribute
    Given there is a {object:node} "node4.attribute.com" with {object:attribute} "bdd4test"
    When REST updates {object:attribute} "bdd4test" on {object:node} "node4.attribute.com"
    Then the page returns "405" result
    Finally REST removes {object:node} "node4.attribute.com"
      And REST removes {object:attribute} "bdd4test"  
