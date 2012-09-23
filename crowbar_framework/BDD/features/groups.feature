Feature: Groups
  In order organize resources
  The system operator, Oscar
  wants to be able to put things into groups

  Scenario: Group Basic
    When AJAX gets the group "bddthings"
    Then the object is properly formatted
      And there should be a key "category"
      And the "groups" object is properly formatted

  Scenario: %Group Add
    Given REST adds the group "simpleadd"
    When AJAX gets the group "simpleadd"
    Then the object is properly formatted
    Finally REST removes the group "simpleadd"

  Scenario: %Group Delete
    Given REST adds the group "simpledelete"
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