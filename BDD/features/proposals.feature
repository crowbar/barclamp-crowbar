Feature: Proposals
  In order to track system configuration
  The system operator, Oscar
  wants to be able to manage proposals

  Scenario: Proposal All Status 
    Skip this test should be going away!
    When AJAX requests the "proposal/status/2.0" page
    Then key "proposals" should contain at least "5" items
      And key "error" should be an empty string
      And key "count" should be a number
      And key "i18n" should contain at least "2" items
      And key "[i18n][unknown]" should be "Unknown, requesting status..."
      
  Scenario: Proposal Single Status 
    Skip this test should be going away!
    When AJAX requests the "proposal/status/2.0/1" page
    Then key "proposals" should contain "1" items
      And key "error" should be an empty string
      And key "[count]" should be "1"
      And key "i18n" should contain "2" items
      And key "[i18n][unknown]" should be "Unknown, requesting status..."
