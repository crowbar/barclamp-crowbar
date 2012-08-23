Feature: Dashboard
  In order monitor the sytem
  The system operator, Oscar
  wants to be able to check the status of nodes

  Scenario: Basic Screen
    When I go to the "dashboard" page
    Then I should see "Node Dashboard"
      And I should see a link to "Add Group"
      And I should see "nodes available in the system"
      And I should see "You may regroup nodes by dragging"
