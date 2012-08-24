Feature: Groups
  In order organize resources
  The system operator, Oscar
  wants to be able to put things into groups

  Scenario: Group Basic
    When AJAX requests the "group/2.0/bddthings?format=json" page
    Then there should be a key "name"
      And there should be a key "description"
      And there should be a key "category"
      And there should be a key "order"
      And the object should comply with API rules

  Scenario: Node Detail using BDD setup
    When AJAX requests the "group/2.0/bddthings?format=json" page
    Then key "name" should be "bddthings"
      And key "category" should be "ui"
      And key "description" should be "BDD Testing Only - should be automatically removed"
      And key "id" should match "node1" from setup
      And key "order" should be "100"
      And the object should comply with API rules