Feature: Groups
  In order organize resources
  The system operator, Oscar
  wants to be able to put things into groups

  Scenario: Node List
    Given REST creates a {object:groups} "nodelisttest"
    When REST gets the {object:groups} list
    Then there should be a value "nodelisttest"
      And there should be a value "bddthings"
    Finally REST removes {object:groups} "nodelisttest"
    
  Scenario: Group Basic
    When REST gets the {object:groups} "bddthings"
    Then the {object:groups} is properly formatted
      And there should be a key "category"

  Scenario: Group Add UI category
    Given there is a "ui" group "simpleadd1"
    When AJAX gets the group "simpleadd1"
    Then the {object:groups} is properly formatted
    Finally REST removes {object:groups} "simpleadd1"

  Scenario: Group Add Rack category
    Given there is a "rack" group "simpleadd2"
    When REST gets the {object:groups} "simpleadd2"
    Then the {object:groups} is properly formatted
    Finally REST removes {object:groups} "simpleadd2"

  Scenario: Group Add Tag category
    Given there is a "tag" group "simpleadd3"
    When REST gets the {object:groups} "simpleadd3"
    Then the {object:groups} is properly formatted
    Finally REST removes {object:groups} "simpleadd3"

  Scenario: Group Delete
    Given there is a "ui" group "simpledelete"
    When REST removes the group "simpledelete"
    Then there is not a "ui" group "simpledelete"

  Scenario: Add Node to Group
    Given REST creates a {object:node} "group1.add.test"
      And there is a "ui" group "add2me"
    When REST adds the node "group1.add.test" to "add2me"
    Then the node "group1.add.test" should be in group "add2me"
      And the group "add2me" should have at least "1" node
    Finally REST removes {object:node} "group1.add.test"
      And throw away group "add2me"
      
  Scenario: Delete Node from Group
    Given REST adds the node "group1.node.test" to "bdddelete"
    When REST removes the node "group1.node.test" from "bdddelete"
    Then the node "group1.node.test" should not be in group "bdddelete"
      And the group "bdddelete" should have "0" nodes
    
  Scenario: Move Node between Groups
    Given there is a "ui" group "end1there"
      And REST adds the node "group1.node.test" to "bddthings"
    When REST moves the node "group1.node.test" from "bddthings" to "end1there"
    Then the node "group1.node.test" should not be in group "bddthings"
      And the node "group1.node.test" should be in group "end1there"
    Finally REST removes the node "group1.node.test" from "end1there"
      And REST removes {object:groups} "end1there"
