Feature: Users
  In order manage users
  The system operator, Oscar
  wants to be able to add, edit and delete users

  Scenario: User Table Renders
    Given I am on the home page 
    When I click on the "Manage Users" menu item
    Then I should see "Manage User Accounts"
      And I should see "Username"
      And I should see "Sign In Count"
      And there should be no translation errors
      
  Scenario: %Nav to Add User Page
    Given I am on the "manage_users" page 
    When I click on the "Add User" link 
    Then I should see "Node Dashboard"
      
  Scenario: %User List
    Given there is a user "oscar"
    When REST gets the user list
    Then there should be a value "crowbar"
      And there should be a value "oscar"
    Finally REST removes the user "oscar"
  
  Scenario: %REST Add User
    Given there is not a user "invisible"
    When REST adds the user "invisible"
    Then there should be a user "invisible"
    Finally REST removes the user "invisible"
    
  