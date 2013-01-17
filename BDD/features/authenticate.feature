Feature: Authentication Works
  In order use the system securely
  The system user, Hacker
  wants to be able to be able to login only with the correct credentials
  
  Scenario: Home Page Redirect to Login
    When I go to home page
    Then I should see "Sign In"
      And I should see "Username"
      And I should see "Password"
          
  Scenario: API Login gives 401
    When I go to node status page
    Then I should get a "401" error
    
  Scenario: digest login with wrong password
    When I login with "wronguser" and "badpassword"
    Then I should see "500 Error"
    
  Scenario: digest login with good password
    When I login with "developer" and "Cr0wbar!"
    Then I should see "User Authenticated using Digest Authentication"
    
  Scenario: Dashboard should redirect
    Skip please fix this test
    When I visit "dashboard" page without login
    Then I should not see "Node Dashboard"
      And I should not see "You are signed in"
      And I should see "Username"
      And I should see "Password"
      
  Scenario: License from Signin
    Skip Rob broken when he moved docs.  Fix after the docs migration
    Given I am on the "my/users/sign_in" page
    When I click on the "License Details" link
    Then I should see "System Licenses"
      And I should see "Crowbar Framework Licenses"
 
  Scenario: Docs Available without Login
    Skip Rob broken when he moved docs.  Fix after the docs migration
    When I visit "docs/topic/crowbar/licenses" page without login
    Then I should see "System Licenses"
      And I should not see "You are signed in"
      