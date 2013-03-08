Feature: Documentation
  In order to learn how to use the system
  The system operator, Oscar
  wants to be able to read about the system in the documentation

  Scenario: Doc Index
    When I go to the "docs" page
    Then I should see heading "System Documentation \\\(Master Index\\\)"
      And I should see "Getting Started with Dell Crowbar"
      And I should see "Crowbar User Guide"
      And I should see "Barclamp Catalog"
      And I should see "Crowbar Deployment Guide"
      And I should see "System Licenses"
      And there should be no translation errors

  Scenario: Doc Topic
    Given parameter "rebuild" is "false"
    Given I am on the "docs" page with parameter "rebuild"
    When I click on the "Crowbar User Guide" link
    Then I should see "Crowbar User Guide"
      And I should see heading "Navigation"
      And I should see heading "Children"
      And there should be no translation errors

  Scenario: Doc Sub Topic
    Given parameter "rebuild" is "false"
    Given I am on the "docs/topic/framework/userguide" page with parameter "rebuild"
    When I click on the "Introduction#" link
    Then I should see heading "Introduction"
      And I should see heading "Navigation"
      And I should see "Master Index"
      And I should see "Crowbar User Guide"
      And there should be no translation errors

  Scenario: Doc Sub Topic return to Main
    Given parameter "rebuild" is "false"
    Given I am on the "docs/topic/framework/userguide" page with parameter "rebuild"
    When I click on the "System Documentation \\\(Master Index\\\)" link
    Then I should see "System Documentation"
      And there should be no translation errors

  Scenario: Doc Export 
    Given parameter "rebuild" is "false"
    Given I am on the "docs/topic/framework/userguide" page with parameter "rebuild"
    When I click on the "Export" link
    Then I should see "Crowbar User Guide"
      And I should see a link to "&lt; Go Back"
      And there should be no translation errors
