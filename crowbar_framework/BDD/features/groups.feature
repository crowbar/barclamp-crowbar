Feature: Groups
  In order organize resources
  The system operator, Oscar
  wants to be able to put things into groups

  Scenario: Group Basic
    When AJAX gets the group "bddthings"
    Then the object is properly formatted
      And there should be a key "category"
      And the "groups" object is properly formatted

  Scenario: Add Node to Group
    Given there is a node "group.add.test"
      And there is a "ui" group "add2me"
    When REST adds the node "group.add.test" to "add2me"
    Then the node "group.add.test" should be in group "add2me"
      And the group "add2me" should have at least "1" node
    Then throw away node "group.add.test"
      And throw away group "add2me"