Feature: Groups
  In order organize resources
  The system operator, Oscar
  wants to be able to put things into groups

  Scenario: Node List
    Given there is a "ui" group "nodelisttest"
    When REST gets the group list
    Then there should be a value "nodelisttest"
      And there should be a value "bddthings"
    Finally throw away group "nodelisttest"
    
  Scenario: Group Basic
    When AJAX gets the group "bddthings"
    Then the object is properly formatted
      And there should be a key "category"
      And the group is properly formatted

  Scenario: Group Add UI category
    Given there is a "ui" group "simpleadd1"
    When AJAX gets the group "simpleadd1"
    Then the group is properly formatted
    Finally throw away group "simpleadd1"

  Scenario: Group Add Rack category
    Given there is a "rack" group "simpleadd2"
    When AJAX gets the group "simpleadd2"
    Then the group is properly formatted
    Finally throw away group "simpleadd2"

  Scenario: Group Add Tag category
    Given there is a "tag" group "simpleadd3"
    When AJAX gets the group "simpleadd3"
    Then the group is properly formatted
    Finally throw away group "simpleadd3"

  Scenario: Group Delete
    Given there is a "ui" group "simpledelete"
    When REST removes the group "simpledelete"
    Then there is not a "ui" group "simpledelete"

  Scenario: Add Node to Group
    Given there is a node "group1.add.test"
      And there is a "ui" group "add2me"
    When REST adds the node "group1.add.test" to "add2me"
    Then the node "group1.add.test" should be in group "add2me"
      And the group "add2me" should have at least "1" node
    Finally throw away node "group1.add.test"
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
      And throw away group "end1there"