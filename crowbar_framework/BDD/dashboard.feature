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

  Scenario: Status Works
    When AJAX requests the "nodes/status" page
    Then key "nodes" should contain at least "1" items
      And key "[nodes][admin][state]" should be "Ready"
      
  Scenario: %I can add a node
    Given there is a node "bdd-test-dashboard-02" in state "testing"
      And pause "10" seconds to "create the node"
    When I go to the "nodes" page
    Then I should see "bdd-test-dashboard-01"

  Scenario: %Check Status
    When I go to the "nodes" page
    Then I should see "a4-ba-db-70-f8-74"
      And I should see "da4-ba-db-17-47-69"
      And I should see "da4-ba-db-17-44-3f"
      And I should see "admin"
  
  Scenario: %Check Link 44-37
    Given I went to the "nodes" page
    When I click on the "da4-ba-db-17-44-3f" link
    Then I should see "Full Name"
      And I should see "da4-ba-db-17-44-3f.dell.com"
      And I should see "Ready"
      And I should see "Allocated"
      And I should see "a4-ba-db-70-f8-74 / 2"
      And I should see "PowerEdge R710"
      And I should see "Intel(R) Xeon(R) CPU E5530 @ 2.40GHz"
      And I should see "47.26 GB"
      And I should see "8"
      And I should see "J2HK5M1"    
    
  Scenario: %Check Link e0-c6
    Given I went to the "nodes" page
    When I click on the "d00-26-9e-cd-e0-c6" link
    Then I should see "Full Name"
      And I should see "d00-26-9e-cd-e0-c6.dell.com"
      And I should see "Ready"
      And I should see "Allocated"
      And I should see "a4-ba-db-88-92-07 / 4"
      And I should see "PowerEdge C2100"
      And I should see "Intel(R) Xeon(R) CPU E5540 @ 2.53GHz"
      And I should see "23.59 GB"
      And I should see "4"
      And I should see "1234567"    

      
  Scenario: %Status Changes
    When node "d00-26-9e-cd-e0-c6.dell.com" status changes to "foo"
      And I go to the "nodes\d00-26-9e-cd-e0-c6.dell.com" page
    Then I should see "foo"