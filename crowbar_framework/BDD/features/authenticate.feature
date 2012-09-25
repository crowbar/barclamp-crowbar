Feature: Authentication Works
  In order use the system securely
  The system user, Hacker
  wants to be able to be able to login only with the correct credentials
  
  Scenario: Home Page Redirect to Login
    When I go to home page
    Then I should see "Sign In"
      And I should see "Username"
      And I should see "Password"

  Scenario: Home Page Redirect
    When I check the home page
    Then I should get a "200" error
      
  Scenario: Digest Login gives 401
    When I go to the digest login page
    Then I should get a "401" error
          
  Scenario: API Login gives 401
    When I go to node status page
    Then I should get a "401" error
    
  Scenario: digest login with wrong password
    When I login with "wronguser" and "badpassword"
    Then I should see "500 Error"
    
  Scenario: digest login with good password
    When I login with "developer" and "Cr0wbar!"
    Then I should see "User Authenticated using Digest Authentication"