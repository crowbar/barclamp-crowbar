Feature: Groups
  In order organize resources
  The system operator, Oscar
  wants to be able to put things into groups

  Scenario: Group Basic
    When AJAX requests the "2.0/group/bddthings" page
    Then the object is properly formatted
      And there should be a key "category"
      And the "groups" object is properly formatted
