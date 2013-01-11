Feature: Documentation
  In order to learn how to use the system
  The system operator, Oscar
  wants to be able to read about the system in the documentation

  Scenario: Doc Index
    When I go to the "docs" page
    Then I should see "System Documentation"
      And I should see "Getting Started with Dell Crowbar"
      And I should see "Crowbar User Guide"
      And I should see "Barclamp Catalog"
      And I should see "Crowbar Deployment Guide"
      And I should not see "System Licenses"
      And there should be no translation errors

  Scenario: Doc Topic
    Given I am on the "docs" page
    When I click on the "Crowbar User Guide" link
    Then I should see "Crowbar User Guide"
      And I should see "Navigation"
      And I should see "Children"
      And there should be no translation errors

  Scenario: Doc Sub Topic
    Given I am on the "docs/topic/crowbar/userguide" page
    When I click on the "Utilities Menu" link
    Then I should see "Utilities Menu"
      And I should see "Navigation"
      And I should see "Children"
      And I should see "Master Index"
      And I should see "Crowbar User Guide"
      And there should be no translation errors

  Scenario: Doc Sub Topic return to Main
    Given I am on the "docs/topic/crowbar/userguide" page
    When I click on the "System Documentation \\\(Master Index\\\)" link
    Then I should see "System Documentation"
      And there should be no translation errors

  Scenario: Doc Export 
    Given I am on the "docs/topic/crowbar/userguide" page
    When I click on the "Export" link
    Then I should see "Crowbar User Guide"
      And I should see a link to "&lt; Go Back"
      And there should be no translation errors
      
  Scenario: %Do NOT show Commented Out
    When I go to the "docs/topic/crowbar/devguide/api/#template" page
    Then I should not see "template" in the body
