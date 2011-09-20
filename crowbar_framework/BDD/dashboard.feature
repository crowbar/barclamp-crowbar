Feature: Dashboard
  In order monitor the sytem
  The system operator, Oscar
  wants to be able to check the status of nodes

  Scenario: I can add a node
    Given there is a node "bdd-test-dashboard-01" in state "testing"
    When I go to the "nodes" page
    Then I should see "bdd-test-dashboard-01"

