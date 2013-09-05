Feature: Crowbar Engine
  In order to use run deployment orchestration
  The system operator, Oscar
  wants step through deployment runs

  Scenario: Deployment Run Step
    Skip WIP ZEHICLE
    Given the "system" deployment has a committed snapshot
    When I run the "system" deployment {int:1} time
    Then I should get an http ok response

  Scenario: Add node into Test Deployment
    Given there is a {o:deployment} "bdd_add_node"
      And there is a {o:node} "bdd-add-me.cr0wbar.com"
      And {o:deployment} "bdd_add_node" includes {o:role} "test-event"
    When I add {o:node} "bdd-add-me.cr0wbar.com" to {o:deployment} "bdd_add_node" in {o:role} "test-event"
    Then the {o:node_role} is properly formatted
      And key "state" is {apply:crowbar.state.proposed}
    Finally REST deletes the {o:deployment} "bdd_add_node"
      And REST deletes the {o:node} "bdd-add-me.cr0wbar.com"