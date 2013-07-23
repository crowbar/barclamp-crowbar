Feature: Crowbar Engine
  In order to use run deployment orchestration
  The system operator, Oscar
  wants step through deployment runs

  Scenario: Deployment Run Step
    Skip WIP ZEHICLE
    Given the "system" deployment has a committed snapshot
    When I run the "system" deployment {integer:1} time
    Then I should get an http ok response
