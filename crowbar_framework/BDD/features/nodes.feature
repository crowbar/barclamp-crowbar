Feature: Nodes
  In order check out the nodes
  The system operator, Oscar
  wants to be able to check the status of nodes

  Scenario: Status Empty
    When AJAX requests the "2.0/status/node" page
    Then key "sum" should be a number
      And there should be a key "state"
      And there should be a key "status"
      And there should be a key "groups"
      And there should be a key "i18n"
      And there should be a key "count"
      And key "[groups][0]" should contain "7" items

  Scenario: Status Non Nodes
    When AJAX requests the "2.0/status/node/0" page
    Then key "sum" should be a number
      And there should be a key "state"
      And there should be a key "status"
      And there should be a key "groups"
      And there should be a key "i18n"
      And there should be a key "count"
      And key "count" should be "0"
      And key "[groups][0]" should contain "7" items

  Scenario: Node Detail
    When I go to the "node/bdd1.example.com" page
    Then I should see "Full Name"
      And I should see "State"
      And I should see "Uptime"
      And I should see "Switch"
      And I should see "MAC Address"
      And I should see "Allocated"
      And I should see "Description"
      And I should see "Memory"
      And I should see "Disk Drives"
      And I should see a link to "Edit"

  Scenario: Node Detail AJAX
    When AJAX requests the "2.0/node/bdd1.example.com" page
    Then key "fingerprint" should be a number
      And there should be a key "state"
      And there should be a key "name"
      And there should be a key "description"
      And there should be a key "created_at"
      And there should be a key "os_id"
      And there should be a key "id"
      And key "id" should be a number
      And there should be a key "order"
      And key "order" should be a number
      And there should be a key "updated_at"
      And the object is properly formatted

  Scenario: Node Detail using BDD setup
    When AJAX requests the "2.0/node/bdd1.example.com" page
    Then key "fingerprint" should be a number
      And there should be a key "state"
      And key "name" should be "bdd1.example.com"
      And key "description" should be "BDD Testing Only - should be automatically removed"
      And key "id" should match "node1" from setup
      And key "id" should be a number
      And key "order" should be "100"
