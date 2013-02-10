Feature: Crowbar CLI
  In order to use the system quickly
  The system operator, Oscar
  wants to use a command line interface

  Scenario: CLI has help
    While local or devtool
    Given CLI is {apply:crowbar.g.cli}
    When I run the "help" command
    Then the CLI should return "Crowbar help"
      And the CLI should return "--username"
      And the CLI should return "--password"    
    
  Scenario: CLI Connects
    While local or devtool
    Given CLI is {apply:crowbar.g.cli}
    When I run the "users" command
    Then the CLI should return "crowbar"
      And the CLI should return "machine_user"
      And the CLI should return "developer"
      
  Scenario: Machines List
    While local or devtool
    Given there is a {object:node} "cli.cr0wbar.com"
    Given CLI is {apply:crowbar.g.cli} 
    When I run the "machines list" command
    Then the CLI should return "cli.cr0wbar.com"
      And the CLI should return "global-node.testing.com""
    Finally REST removes {object:node} "cli.cr0wbar.com"