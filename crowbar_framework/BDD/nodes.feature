Feature: Nodes
  In order check out the nodes
  The system operator, Oscar
  wants to be able to check the status of nodes

  Scenario: Status Empty
    When AJAX requests the "node/status/2.0" page
    Then key "sum" should be a number
      And there should be a key "[state]"
      And there should be a key "[status]"
      And there should be a key "[groups]"
      And there should be a key "[i18n]"

{"sum":0,"state":{},"groups":{},"i18n":{},"status":{},"count":0}

  Scenario: %Status Works
    When AJAX requests the "nodes/status" page
    Then key "nodes" should contain at least "1" items
      And key "[nodes][admin][state]" should be "Ready"
      
  Scenario: %I can add a node
    Given there is a node "bdd-test-dashboard-02" in state "testing"
      And pause "10" seconds to "create the node"
    When I go to the "nodes" page
    Then I should see "bdd-test-dashboard-01"
      
  Scenario: %Status Changes
    When node "d00-26-9e-cd-e0-c6.dell.com" status changes to "foo"
      And I go to the "nodes\d00-26-9e-cd-e0-c6.dell.com" page
    Then I should see "foo"